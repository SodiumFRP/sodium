package sodium

import sodium.Node.Target

import scala.collection.mutable.ListBuffer

class Stream[A] private (val node: Node,
                         protected val finalizers: ListBuffer[Listener],
                         protected val firings: ListBuffer[A]) {
  import Stream._

  def this() = this(new Node(0L), ListBuffer[Listener](), ListBuffer[A]())

  /**
    * Listen for firings of this event. The returned Listener has an unlisten()
    * method to cause the listener to be removed. This is the observer pattern.
    */
  final def listen(action: A => Unit): Listener =
    listen_(Node.NullNode, new TransactionHandler[A]() {
      def run(trans2: Transaction, a: A): Unit = {
        action(a)
      }
    })

  final def listen_(target: Node, action: TransactionHandler[A]): Listener =
    Transaction(trans1 => listen(target, trans1, action, false))

  def listen(target: Node,
             trans: Transaction,
             action: TransactionHandler[A],
             suppressEarlierFirings: Boolean): Listener = {
    val node_target_ = new Array[Target](1)
    Transaction.listenersLock.synchronized {
      if (node.linkTo(action.asInstanceOf[TransactionHandler[Unit]], target, node_target_))
        trans.toRegen = true
    }
    val node_target = node_target_(0)
    val firings = this.firings.clone() //TODO check if deep clone is needed
    if (!suppressEarlierFirings && !firings.isEmpty) {
      trans.prioritized(
        target, { trans2 =>
          // Anything sent already in this transaction must be sent now so that
          // there's no order dependency between send and listen.
          firings.foreach { a =>
            Transaction.inCallback += 1
            try { // Don't allow transactions to interfere with Sodium internals.
              action.run(trans, a)
            } catch {
              case t: Throwable => t.printStackTrace()
            } finally {
              Transaction.inCallback -= 1
            }
          }
        }
      )
    }
    new ListenerImplementation[A](this, action, node_target)
  }

  /**
    * Transform the event's value according to the supplied function.
    */
  final def map[B](f: A => B): Stream[B] = {
    val out = new StreamSink[B]
    val l = listen_(out.node, new TransactionHandler[A]() {
      override def run(trans: Transaction, a: A): Unit = {
        out.send(trans, f(a))
      }
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Create a behavior with the specified initial value, that gets updated
    * by the values coming through the event. The 'current value' of the behavior
    * is notionally the value as it was 'at the start of the transaction'.
    * That is, state updates caused by event firings get processed at the end of
    * the transaction.
    */
  final def hold(initValue: A): Cell[A] =
    Transaction(trans => new Cell[A](lastFiringOnly(trans), initValue))

  final def holdLazy(initValue: Lazy[A]): Cell[A] =
    Transaction(trans => holdLazy(trans, initValue))

  final def holdLazy(trans: Transaction, initValue: Lazy[A]): Cell[A] =
    new LazyCell[A](lastFiringOnly(trans), Some(initValue))

  /**
    * Variant of snapshot that throws away the event's value and captures the behavior's.
    */
  final def snapshot[B](beh: Cell[B]): Stream[B] = snapshot[B, B](beh, (a, b) => b)

  /**
    * Sample the behavior at the time of the event firing. Note that the 'current value'
    * of the behavior that's sampled is the value as at the start of the transaction
    * before any state changes of the current transaction are applied through 'hold's.
    */
  final def snapshot[B, C](b: Cell[B], f: (A, B) => C): Stream[C] = {
    val out = new StreamSink[C]()
    val l = listen_(out.node, new TransactionHandler[A]() {
      def run(trans: Transaction, a: A): Unit = {
        out.send(trans, f(a, b.sampleNoTrans()))
      }
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Merge two streams of events of the same type.
    *
    * In the case where two event occurrences are simultaneous (i.e. both
    * within the same transaction), both will be delivered in the same
    * transaction. If the event firings are ordered for some reason, then
    * their ordering is retained. In many common cases the ordering will
    * be undefined.
    */
  def merge(eb: Stream[A]): Stream[A] = Stream.merge[A](this, eb)

  /**
    * Push each event occurrence onto a new transaction. Same as split() but works on a single value.
    */
  final def defer(): Stream[A] = {
    val out = new StreamSink[A]()
    val l = listen_(
      out.node,
      new TransactionHandler[A]() {
        def run(trans: Transaction, a: A): Unit = {
          trans.post(new Runnable() {
            def run(): Unit = {
              val trans = new Transaction()
              try {
                out.send(trans, a)
              } finally {
                trans.close()
              }
            }
          })
        }
      }
    )
    out.unsafeAddCleanup(l)
  }

  /**
    * Push each event occurrence in the list onto a new transaction.
    */
  def split[C <: Traversable[A]](s: Stream[C]): Stream[A] = {
    val out = new StreamSink[A]
    val l1 = s.listen_(
      out.node,
      new TransactionHandler[C]() {
        override def run(trans: Transaction, as: C): Unit = {
          trans.post(new Runnable() {
            override def run(): Unit = {
              for (a <- as) {
                val trans = new Transaction()
                try out.send(trans, a)
                finally trans.close()
              }
            }
          })
        }
      }
    )
    out.unsafeAddCleanup(l1)
  }

  /**
    * If there's more than one firing in a single transaction, combine them into
    * one using the specified combining function.
    *
    * If the event firings are ordered, then the first will appear at the left
    * input of the combining function. In most common cases it's best not to
    * make any assumptions about the ordering, and the combining function would
    * ideally be commutative.
    */
  final def coalesce(f: (A, A) => A): Stream[A] =
    Transaction(trans => coalesce(trans, f))

  final def coalesce(trans: Transaction, f: (A, A) => A): Stream[A] = {
    val out = new StreamSink[A]()
    val l = listen(
      out.node,
      trans,
      new TransactionHandler[A]() {
        private var acc: Option[A] = None
        override def run(trans1: Transaction, a: A): Unit = {
          acc match {
            case Some(b) =>
              acc = Some(f(b, a))
            case None =>
              trans1.prioritized(out.node, { trans2 =>
                out.send(trans2, acc.get)
                acc = None
              })
              acc = Some(a)
          }
        }
      },
      false
    )
    out.unsafeAddCleanup(l)
  }

  /**
    * Clean up the output by discarding any firing other than the last one.
    */
  final def lastFiringOnly(trans: Transaction): Stream[A] =
    coalesce(trans, (first, second) => second)

  /**
    * Merge two streams of events of the same type, combining simultaneous
    * event occurrences.
    *
    * In the case where multiple event occurrences are simultaneous (i.e. all
    * within the same transaction), they are combined using the same logic as
    * 'coalesce'.
    */
  def merge(eb: Stream[A], f: (A, A) => A): Stream[A] =
    merge(eb).coalesce(f)

  /**
    * Only keep event occurrences for which the predicate returns true.
    */
  def filter(f: A => Boolean): Stream[A] = {
    val out = new StreamSink[A]()
    val l = listen_(out.node, new TransactionHandler[A]() {
      def run(trans: Transaction, a: A): Unit = {
        if (f(a)) out.send(trans, a)
      }
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Filter out any event occurrences whose value is a Java null pointer.
    */
  final def filterNotNull(): Stream[A] = filter(_ != null)

  /**
    * Let event occurrences through only when the behavior's value is True.
    * Note that the behavior's value is as it was at the start of the transaction,
    * that is, no state changes from the current transaction are taken into account.
    */
  final def gate(bPred: Cell[Boolean]): Stream[A] =
    filterOption(snapshot[Boolean, Option[A]](bPred, (a, pred) => if (pred) Some(a) else None))

  /**
    * Transform an event with a generalized state loop (a mealy machine). The function
    * is passed the input and the old state and returns the new state and output value.
    */
  final def collect[B, S](initState: S, f: (A, S) => (B, S)): Stream[B] = collectLazy(new Lazy[S](initState), f)

  /**
    * Transform an event with a generalized state loop (a mealy machine). The function
    * is passed the input and the old state and returns the new state and output value.
    */
  final def collectLazy[B, S](initState: Lazy[S], f: (A, S) => (B, S)): Stream[B] =
    Transaction(_ => {
      val es = new StreamLoop[S]()
      val s = es.holdLazy(initState)
      val ebs = snapshot(s, f)
      val eb = ebs.map(bs => bs._1)
      val es_out = ebs.map(bs => bs._2)
      es.loop(es_out)
      eb
    })

  /**
    * Accumulate on input event, outputting the new state each time.
    */
  final def accum[S](initState: S, f: (A, S) => S): Cell[S] = accumLazy(new Lazy[S](initState), f)

  /**
    * Accumulate on input event, outputting the new state each time.
    * Variant that takes a lazy initial state.
    */
  final def accumLazy[S](initState: Lazy[S], f: (A, S) => S): Cell[S] =
    Transaction(_ => {
      val es = new StreamLoop[S]()
      val s = es.holdLazy(initState)
      val es_out = snapshot(s, f)
      es.loop(es_out)
      es_out.holdLazy(initState)
    })

  /**
    * Throw away all event occurrences except for the first one.
    */
  final def once(): Stream[A] = {
    // This is a bit long-winded but it's efficient because it deregisters
    // the listener.
    val ev = this
    var la: Option[Listener] = None
    val out = new StreamSink[A]()
    la = Some(
      ev.listen_(
        out.node,
        new TransactionHandler[A]() {
          def run(trans: Transaction, a: A): Unit = {
            if (la.isDefined) {
              out.send(trans, a)
              la.foreach(_.unlisten())
              la = None
            }
          }
        }
      ))
    out.unsafeAddCleanup(la.get)
  }

  def unsafeAddCleanup(cleanup: Listener): Stream[A] = {
    finalizers += cleanup
    this
  }

  def addCleanup(cleanup: Listener): Stream[A] = {
    val fsNew: ListBuffer[Listener] = finalizers
    fsNew += cleanup
    new Stream[A](node, fsNew, firings)
  }

  protected override def finalize(): Unit = {
    finalizers.foreach(_.unlisten)
  }
}

object Stream {

  final class ListenerImplementation[A](var event: Stream[A], var action: TransactionHandler[A], var target: Target)
      extends Listener {

    /**
      * It's essential that we keep the listener alive while the caller holds
      * the Listener, so that the finalizer doesn't get triggered.
      */
    /**
      * It's also essential that we keep the action alive, since the node uses
      * a weak reference.
      */
    override def unlisten(): Unit = {
      Transaction.listenersLock.synchronized {
        if (this.event != null) {
          event.node.unlinkTo(target)
          event = null
          action = null
          target = null
        }
      }
    }

  }

  /**
    * Merge two streams of events of the same type.
    *
    * In the case where two event occurrences are simultaneous (i.e. both
    * within the same transaction), both will be delivered in the same
    * transaction. If the event firings are ordered for some reason, then
    * their ordering is retained. In many common cases the ordering will
    * be undefined.
    */
  private def merge[A](ea: Stream[A], eb: Stream[A]): Stream[A] = {
    val out = new StreamSink[A]()
    val left = new Node(0)
    val right = out.node
    val node_target_ = new Array[Target](1)
    left.linkTo(null, right, node_target_)
    val node_target = node_target_(0)
    val h = new TransactionHandler[A]() {
      def run(trans: Transaction, a: A): Unit = {
        out.send(trans, a)
      }
    }
    val l1 = ea.listen_(left, h)
    val l2 = eb.listen_(right, h)
    out
      .unsafeAddCleanup(l1)
      .unsafeAddCleanup(l2)
      .unsafeAddCleanup(new Listener() {
        def unlisten(): Unit = {
          left.unlinkTo(node_target)
        }
      })

  }

  /**
    * Filter the empty values out, and strip the Option wrapper from the present ones.
    */
  final def filterOption[A](ev: Stream[Option[A]]): Stream[A] = {
    val out = new StreamSink[A]()
    val l = ev.listen_(out.node, new TransactionHandler[Option[A]]() {
      def run(trans: Transaction, oa: Option[A]): Unit = {
        oa.foreach(out.send(trans, _))
      }
    })
    out.unsafeAddCleanup(l)
  }

}
