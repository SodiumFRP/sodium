package sodium

import scala.IndexedSeq
import scala.collection.mutable.ListBuffer

class Stream[A] {
  import Stream._

  protected val listeners = ListBuffer[TransactionHandler[A]]()
  protected val finalizers = ListBuffer[Listener]()
  val node = new Node(0L)
  protected val firings = ListBuffer[A]()

  def sampleNow(): IndexedSeq[A] = IndexedSeq()

  /**
   * Listen for firings of this event. The returned Listener has an unlisten()
   * method to cause the listener to be removed. This is the observer pattern.
   */
  final def listen(action: A => Unit): Listener =
    listen_(Node.NullNode, new TransactionHandler[A]() {
      def run(trans2: Transaction, a: A) {
        action(a)
      }
    })

  final def listen_(target: Node, action: TransactionHandler[A]): Listener =
    Transaction(trans1 => listen(target, trans1, action, false))

  def listen(target: Node, trans: Transaction, action: TransactionHandler[A],
    suppressEarlierFirings: Boolean): Listener = {
    Transaction.listenersLock.synchronized {
      if (node.linkTo(target))
        trans.toRegen = true
      listeners += action
    }
    trans.prioritized(target, {
      trans2 =>
        sampleNow().foreach(action.run(trans, _))
        if (!suppressEarlierFirings) {
          // Anything sent already in this transaction must be sent now so that
          // there's no order dependency between send and listen.
          firings.foreach(action.run(trans, _))
        }
    })
    new ListenerImplementation[A](this, action, target)
  }

  /**
   * Transform the event's value according to the supplied function.
   */
  final def map[B](f: A => B): Stream[B] = {
    val ev = this
    val out = new StreamSink[B]() {
      override def sampleNow() = ev.sampleNow().map(f(_))
    }
    val l = listen_(out.node, new TransactionHandler[A]() {
      override def run(trans: Transaction, a: A) {
        out.send(trans, f(a))
      }
    })
    out.addCleanup(l)
  }

  /**
   * Create a behavior with the specified initial value, that gets updated
   * by the values coming through the event. The 'current value' of the behavior
   * is notionally the value as it was 'at the start of the transaction'.
   * That is, state updates caused by event firings get processed at the end of
   * the transaction.
   */
  final def hold(initValue: A): Cell[A] =
    Transaction(trans => new Cell[A](initValue, lastFiringOnly(trans)))

  final def holdLazy(initValue: () => A): Cell[A] =
    Transaction(trans => new LazyCell[A](initValue, lastFiringOnly(trans)))

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
    val ev = this
    val out = new StreamSink[C]() {
      override def sampleNow() = ev.sampleNow().map(a => f.apply(a, b.sampleNoTrans()))
    }
    val l = listen_(out.node, new TransactionHandler[A]() {
      def run(trans: Transaction, a: A) {
        out.send(trans, f(a, b.sampleNoTrans()))
      }
    })
    out.addCleanup(l)
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
   * Push each event occurrence onto a new transaction.
   */
  final def delay(): Stream[A] =
    {
      val out = new StreamSink[A]()
      val l = listen_(out.node, new TransactionHandler[A]() {
        def run(trans: Transaction, a: A) {
          trans.post(new Runnable() {
            def run() {
              val trans = new Transaction()
              try {
                out.send(trans, a)
              } finally {
                trans.close()
              }
            }
          })
        }
      })
      out.addCleanup(l)
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

  final def coalesce(trans: Transaction, f: (A, A) => A): Stream[A] =
    {
      val ev = this
      val out = new StreamSink[A]() {
        override def sampleNow() = {
          val oi = ev.sampleNow()
          if (oi.isEmpty) {
            IndexedSeq()
          } else {
            IndexedSeq(oi.reduce(f(_, _)))
          }
        }
      }
      val l = listen(out.node, trans, new TransactionHandler[A]() {
        private var acc: Option[A] = None
        override def run(trans1: Transaction, a: A) {
          acc match {
            case Some(b) =>
              acc = Some(f(b, a))
            case None =>
              trans1.prioritized(out.node, {
                trans2 =>
                  out.send(trans2, acc.get)
                  acc = None
              })
              acc = Some(a)
          }
        }
      }, false)
      out.addCleanup(l)
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
    val ev = this
    val out = new StreamSink[A]() {
      override def sampleNow() = ev.sampleNow().filter(f)
    }
    val l = listen_(out.node, new TransactionHandler[A]() {
      def run(trans: Transaction, a: A) {
        if (f(a)) out.send(trans, a)
      }
    })
    out.addCleanup(l)
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
  final def collect[B, S](initState: S, f: (A, S) => (B, S)): Stream[B] =
    Transaction(_ => {
      val es = new StreamLoop[S]()
      val s = es.hold(initState)
      val ebs = snapshot(s, f)
      val eb = ebs.map(bs => bs._1)
      val es_out = ebs.map(bs => bs._2)
      es.loop(es_out)
      eb
    })

  /**
   * Accumulate on input event, outputting the new state each time.
   */
  final def accum[S](initState: S, f: (A, S) => S): Cell[S] =
    Transaction(_ => {
      val es = new StreamLoop[S]()
      val s = es.hold(initState)
      val es_out = snapshot(s, f)
      es.loop(es_out)
      es_out.hold(initState)
    })

  /**
   * Throw away all event occurrences except for the first one.
   */
  final def once(): Stream[A] = {
    // This is a bit long-winded but it's efficient because it deregisters
    // the listener.
    val ev = this
    var la: Option[Listener] = None
    val out = new StreamSink[A]() {
      override def sampleNow() = {
        val oi = ev.sampleNow()
        if (oi.size > 0) {
          la.foreach(_.unlisten())
          la = None
        }
        if (oi.size > 0) IndexedSeq(oi.head) else IndexedSeq()
      }
    }
    la = Some(ev.listen_(out.node, new TransactionHandler[A]() {
      def run(trans: Transaction, a: A) {
        out.send(trans, a)
        if (la.isDefined) {
          la.foreach(_.unlisten())
          la = None
        }
      }
    }))
    out.addCleanup(la.get)
  }

  def addCleanup(cleanup: Listener): Stream[A] = {
    finalizers += cleanup
    this
  }

  protected override def finalize() {
    finalizers.foreach(_.unlisten)
  }
}

object Stream {
  final class ListenerImplementation[A](
    val event: Stream[A],
    val action: TransactionHandler[A],
    val target: Node) extends Listener {

    /**
     * It's essential that we keep the listener alive while the caller holds
     * the Listener, so that the finalizer doesn't get triggered.
     */

    override def unlisten() {
      Transaction.listenersLock.synchronized {
        event.listeners -= action
        event.node.unlinkTo(target)
      }
    }

    override protected def finalize() {
      unlisten()
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
  private def merge[A](ea: Stream[A], eb: Stream[A]): Stream[A] =
    {
      val out = new StreamSink[A]() {
        override def sampleNow() = ea.sampleNow() ++ eb.sampleNow()
      }
      val l1 = ea.listen_(out.node, new TransactionHandler[A]() {
        def run(trans: Transaction, a: A) {
          out.send(trans, a)
        }
      })
      val l2 = eb.listen_(out.node, new TransactionHandler[A]() {
        def run(trans1: Transaction, a: A) {
          trans1.prioritized(out.node, trans2 => out.send(trans2, a))
        }
      })
      out.addCleanup(l1).addCleanup(l2)
    }

  /**
   * Filter the empty values out, and strip the Option wrapper from the present ones.
   */
  final def filterOption[A](ev: Stream[Option[A]]): Stream[A] =
    {
      val out = new StreamSink[A]() {
        override def sampleNow() = ev.sampleNow().flatten
      }
      val l = ev.listen_(out.node, new TransactionHandler[Option[A]]() {
        def run(trans: Transaction, oa: Option[A]) {
          oa.foreach(out.send(trans, _))
        }
      })
      out.addCleanup(l)
    }

}
