package sodium

import sodium.Node.Target

import scala.collection.mutable.ListBuffer

/**
  * Represents a stream of discrete events/firings containing values of type A.
  */
class Stream[A] private (val node: Node, val finalizers: ListBuffer[Listener], val firings: ListBuffer[A]) {
  import Stream._

  /**
    * A stream that never fires.
    */
  def this() = this(new Node(0L), ListBuffer[Listener](), ListBuffer[A]())

  /**
    * Listen for events/firings on this stream. This is the observer pattern. The
    * returned [[Listener]] has a [[sodium.Listener.unlisten()* Listenerunlisten()]] method to cause the
    * listener to be removed. This is an OPERATIONAL mechanism is for interfacing between
    * the world of I/O and for FRP.
    *
    * @param action The handler to execute when there's a new value.
    *               You should make no assumptions about what thread you are called on, and the
    *               handler should not block. You are not allowed to use [[sodium.CellSink.send CellSink.send(A)]]
    *               or [[sodium.StreamSink.send(a:A):Unit* StreamSink.send(A)]] in the handler.
    *               An exception will be thrown, because you are not meant to use this to create
    *               your own primitives.
    */
  final def listen(action: A => Unit): Listener =
    listen_(Node.NullNode, (trans2: Transaction, a: A) => {
      action(a)
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
              action.run(trans2, a)
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
    * Transform the stream's event values according to the supplied function, so the returned
    * Stream's event values reflect the value of the function applied to the input
    * Stream's event values.
    *
    * @param f Function to apply to convert the values. It may construct FRP logic or use
    *          [[sodium.Cell.sample():A* Cell.sample()]] in which case it is equivalent to
    *          [[sodium.Stream.snapshot[B]* Stream.snapshot(Cell)]]ing the
    *          cell. Apart from this the function must be <em>referentially transparent</em>.
    */
  final def map[B](f: A => B): Stream[B] = {
    val out = new StreamSink[B]
    val l = listen_(out.node, (trans: Transaction, a: A) => {
      out.send(trans, f(a))
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Create a [[Cell]] with the specified initial value, that is updated
    * by this stream's event values.
    *
    * There is an implicit delay: State updates caused by event firings don't become
    * visible as the cell's current value as viewed by [[sodium.Stream.snapshot[B,C]* Stream.snapshot(Cell,(A,B)=>C)]]
    * until the following transaction. To put this another way,
    * [[sodium.Stream.snapshot[B,C]* Stream.snapshot(Cell,(A,B)=>C)]] always sees the value of a cell as it was before
    * any state changes from the current transaction.
    */
  final def hold(initValue: A): Cell[A] =
    Transaction(trans => new Cell[A](lastFiringOnly(trans), initValue))

  /**
    * A variant of [[sodium.Stream.hold(initValue:A)* hold(A)]] with an initial value captured by
      [[Cell.sampleLazy()* Cell.sampleLazy()]].
    */
  final def holdLazy(initValue: Lazy[A]): Cell[A] =
    Transaction(trans => holdLazy(trans, initValue))

  final def holdLazy(trans: Transaction, initValue: Lazy[A]): Cell[A] =
    new LazyCell[A](lastFiringOnly(trans), Some(initValue))

  /**
    * Variant of [[sodium.Stream.snapshot[B,C]* Stream.snapshot(Cell,(A,B)=>C)]] that captures the cell's value
    * at the time of the event firing, ignoring the stream's value.
    */
  final def snapshot[B](beh: Cell[B]): Stream[B] = snapshot[B, B](beh, (a, b) => b)

  /**
    * Return a stream whose events are the result of the combination using the specified
    * function of the input stream's event value and the value of the cell at that time.
    *
    * There is an implicit delay: State updates caused by event firings being held with
    * [[sodium.Stream.hold(initValue:A)* hold(A)]] don't become visible as the cell's current value until the
    * following transaction. To put this another way, [[sodium.Stream.snapshot[B,C]* Stream.snapshot(Cell,(A,B)=>C)]]
    * always sees the value of a cell as it was before any state changes from the current
    * transaction.
    */
  final def snapshot[B, C](c: Cell[B], f: (A, B) => C): Stream[C] = {
    val out = new StreamSink[C]()
    val l = listen_(out.node, (trans: Transaction, a: A) => {
      out.send(trans, f(a, c.sampleNoTrans()))
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Merge two streams of the same type into one, so that events on either input appear
    * on the returned stream.
    *
    * In the case where two events are simultaneous (i.e. both
    * within the same transaction), the event from <em>s</em> will take precedence, and
    * the event from <em>this</em> will be dropped.
    * If you want to specify your own combining function, use
    * [[Stream!.merge(s:sodium\.Stream[A],f:(A,A)=>A):sodium\.Stream[A]* Stream.merge(Stream,(A,A)=>A)]].
    * merge(s) is equivalent to merge(s, (l, r) -&gt; r).
    */
  def merge(s: Stream[A]): Stream[A] = merge(s, (left, right) => right)

  /**
    * A variant of [[sodium.Stream.merge(s:sodium\.Stream[A]):sodium\.Stream[A]* merge(Stream)]] that uses the specified
    * function to combine simultaneous events.
    *
    * If the events are simultaneous (that is, one event from this and one from <em>s</em>
    * occurring in the same transaction), combine them into one using the specified combining function
    * so that the returned stream is guaranteed only ever to have one event per transaction.
    * The event from <em>this</em> will appear at the left input of the combining function, and
    * the event from <em>s</em> will appear at the right.
    *
    * @param f Function to combine the values. It may construct FRP logic or use
    *          [[sodium.Cell.sample():A* Cell.sample()]].
    *          Apart from this the function must be <em>referentially transparent</em>.
    */
  def merge(s: Stream[A], f: ((A, A) => A)): Stream[A] = {
    Transaction(trans => merge_(this, s).coalesce(trans, f))
  }

  /**
    * Coalesce simultaneous events on a single stream into one. This is only useful in the
    * situation where [[sodium.StreamSink.send(a:A):Unit* StreamSink.send(A)]] has been called multiple times in a
    * transaction on the same [[StreamSink]]. The combining function should be <em>associative</em>.
    *
    * @param f Function to combine the values. It may construct FRP logic or use
    *           [[sodium.Cell.sample():A* Cell.sample()]].
    *           Apart from this the function must be <em>referentially transparent</em>.
    */
  final def coalesce(f: (A, A) => A): Stream[A] = {
    Transaction(trans => coalesce(trans, f))
  }

  final def coalesce(trans1: Transaction, f: (A, A) => A): Stream[A] = {
    val out = new StreamSink[A]()
    val l = listen(
      out.node,
      trans1,
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
    * Push each event onto a new transaction guaranteed to come before the next externally
    * initiated transaction. Same as [[sodium.Stream.split split(Stream)]] but it works on a single value.
    */
  final def defer(): Stream[A] = {
    val out = new StreamSink[A]()
    val l = listen_(
      out.node,
      (trans: Transaction, a: A) => {
        trans.post_(() => {
          val trans = new Transaction()
          try {
            out.send(trans, a)
          } finally {
            trans.close()
          }
        })
      }
    )
    out.unsafeAddCleanup(l)
  }

  /**
    * Clean up the output by discarding any firing other than the last one.
    */
  final def lastFiringOnly(trans: Transaction): Stream[A] =
    coalesce(trans, (first, second) => second)

  /**
    * Return a stream that only outputs events for which the predicate returns true.
    */
  def filter(predicate: A => Boolean): Stream[A] = {
    val out = new StreamSink[A]()
    val l = listen_(out.node, (trans: Transaction, a: A) => {
      if (predicate(a)) out.send(trans, a)
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Return a stream that only outputs event occurrences from the input stream
    * when the specified cell's value is true.
    */
  final def gate(c: Cell[Boolean]): Stream[A] =
    filterOptional(snapshot[Boolean, Option[A]](c, (a, pred) => if (pred) Some(a) else None))

  /**
    * Transform an event with a generalized state loop (a Mealy machine). The function
    * is passed the input and the old state and returns the new state and output value.
    *
    * @param f Function to apply to update the state. It may construct FRP logic or use
    *          [[sodium.Cell.sample():A* Cell.sample()]] in which case it is equivalent to
    *          [[sodium.Stream.snapshot[B]* Stream.snapshot(Cell)]]ing the
    *          cell. Apart from this the function must be <em>referentially transparent</em>.
    */
  final def collect[B, S](initState: S, f: (A, S) => (B, S)): Stream[B] = collectLazy(new Lazy[S](initState), f)

  /**
    * A variant of [[sodium.Stream.collect[B,S](initState:S,f:(A,S)=>(B,S)):sodium\.Stream[B]* collect(S,(A,S)=>(B,S)]]
    * that takes an initial state returned by [[Cell.sampleLazy()* Cell.sampleLazy()]].
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
    *
    * @param f Function to apply to update the state. It may construct FRP logic or use
    *          [[sodium.Cell.sample():A* Cell.sample()]] in which case it is equivalent to
    *          [[sodium.Stream.snapshot[B]* Stream.snapshot(Cell)]]ing the
    *          cell. Apart from this the function must be <em>referentially transparent</em>.
    */
  final def accum[S](initState: S, f: (A, S) => S): Cell[S] = accumLazy(new Lazy[S](initState), f)

  /**
    * A variant of [[sodium.Stream.accum[S](initState:S,f:(A,S)=>S):sodium\.Cell[S]* accum(S,(A,S)=>S)]] that takes an
    *  initial state returned by [[Cell.sampleLazy()* Cell.sampleLazy()]].
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
    * Return a stream that outputs only one value: the next event occurrence of the
    * input stream.
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
        (trans: Transaction, a: A) => {
          if (la.isDefined) {
            out.send(trans, a)
            la.foreach(_.unlisten())
            la = None
          }
        }
      ))
    out.unsafeAddCleanup(la.get)
  }

  /**
    * This is not thread-safe, so one of these two conditions must apply:
    1. We are within a transaction, since in the current implementation a transaction locks out all other threads.
    1. The object on which this is being called was created has not yet
       been returned from the method where it was created, so it can't be shared between threads.
    */
  def unsafeAddCleanup(cleanup: Listener): Stream[A] = {
    finalizers += cleanup
    this
  }

  /**
    * Attach a listener to this stream so that its [[sodium.Listener.unlisten()* Listener.unlisten()]] is invoked
    * when this stream is garbage collected. Useful for functions that initiate I/O,
    * returning the result of it through a stream.
    */
  def addCleanup(cleanup: Listener): Stream[A] =
    Transaction(trans => {
      val fsNew: ListBuffer[Listener] = finalizers
      fsNew += cleanup
      new Stream[A](node, fsNew, firings)
    })

  protected override def finalize(): Unit = {
    finalizers.foreach(_.unlisten)
  }
}

object Stream {

  /**
    * It's essential that we keep the listener alive while the caller holds
    * the Listener, so that the finalizer doesn't get triggered.
    * It's also essential that we keep the action alive, since the node uses
    * a weak reference.
    */
  final class ListenerImplementation[A](var event: Stream[A], var action: TransactionHandler[A], var target: Target)
      extends Listener {

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
  private def merge_[A](ea: Stream[A], eb: Stream[A]): Stream[A] = {
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
    * Return a stream that only outputs events that have present
    * values, removing the scala.Option wrapper, discarding empty values.
    */
  final def filterOptional[A](ev: Stream[Option[A]]): Stream[A] = {
    val out = new StreamSink[A]()
    val l = ev.listen_(out.node, (trans: Transaction, oa: Option[A]) => {
      oa.foreach(out.send(trans, _))
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Push each event in the list onto a newly created transaction guaranteed
    * to come before the next externally initiated transaction.
    */
  final def split[A, C <: Traversable[A]](s: Stream[C]): Stream[A] = {
    val out = new StreamSink[A]
    val l1 = s.listen_(
      out.node,
      (trans: Transaction, as: C) => {
        trans.post_(() => {
          for (a <- as) {
            val trans = new Transaction()
            try out.send(trans, a)
            finally trans.close()
          }
        })
      }
    )
    out.unsafeAddCleanup(l1)
  }

}
