package sodium

import sodium.Node.Target

import scala.collection.mutable
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
    * @param handler The handler to execute when there's a new value.
    *               You should make no assumptions about what thread you are called on, and the
    *               handler should not block. You are not allowed to use [[sodium.BehaviorSink.send BehaviorSink.send(A)]]
    *               or [[sodium.StreamSink.send(a:A):Unit* StreamSink.send(A)]] in the handler.
    *               An exception will be thrown, because you are not meant to use this to create
    *               your own primitives.
    */
  final def listen(handler: A => Unit): Listener = {
    val l0: Listener = listenWeak(handler)
    val l = new Listener {
      override def unlisten(): Unit = {
        l0.unlisten()
        keepListenersAlive.synchronized {
          keepListenersAlive.remove(this)
          ()
        }
      }
    }
    keepListenersAlive.synchronized {
      keepListenersAlive.add(l)
    }
    l
  }

  /**
    * A variant of [[sodium.Stream.listen(handler:A=>Unit):sodium\.Listener* listen(A=>Unit)]]  that handles the first
    * event and then automatically deregisters itself. This is useful for implementing things that work like promises.
    */
  def listenOnce(handler: A => Unit): Listener = {
    val lRef = new Array[Listener](1)
    lRef(0) = listen(a => {
      lRef(0).unlisten()
      handler(a)
    })
    lRef(0)
  }

  final def listen_(target: Node, action: TransactionHandler[A]): Listener =
    Transaction(trans1 => listen(target, trans1, action, false))

  /**
    * A variant of [[Stream.listen(handler:A=>Unit):sodium\.Listener* listen(A=>Unit)]] that will deregister the
    * listener automatically if the listener is garbage collected.
    * With [[Stream.listen(handler:A=>Unit):sodium\.Listener* listen(A=>Unit)]], the listener is
    * only deregistered if [[sodium.Listener.unlisten()* Listenerunlisten()]] is called explicitly.
    *
    * This method should be used for listeners that are to be passed to
    * [[sodium.Stream.addCleanup* Stream.addCleanup(Listener]] to ensure that things don't get kept alive
    * when they shouldn't.
    */
  final def listenWeak(action: A => Unit): Listener =
    listen_(Node.NullNode, (_: Transaction, a: A) => {
      action(a)
    })

  def listen(target: Node,
             trans: Transaction,
             action: TransactionHandler[A],
             suppressEarlierFirings: Boolean): Listener = {
    val node_target_ = new Array[Target](1)
    Transaction.listenersLock.synchronized {
      if (node.linkTo(action.asInstanceOf[TransactionHandler[Unit]], target, node_target_)) {
        trans.toRegen = true
      }
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
    *          [[sodium.Behavior.sample():A* Behavior.sample()]] in which case it is equivalent to
    *          [[sodium.Stream.snapshot[B]* Stream.snapshot(Behavior)]]ing the
    *          cell. Apart from this the function must be <em>referentially transparent</em>.
    */
  final def map[B](f: A => B): Stream[B] = {
    val out = new StreamWithSend[B]()
    val l = listen_(out.node, (trans: Transaction, a: A) => {
      out.send(trans, f(a))
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Transform the stream's event values into the specified constant value.
    *
    * @param b Constant value.
    */
  final def mapTo[B](b: B): Stream[B] = this.map(_ => b)

  /**
    * Create a [[Behavior]] with the specified initial value, that is updated
    * by this stream's event values.
    *
    * There is an implicit delay: State updates caused by event firings don't become
    * visible as the cell's current value as viewed by [[sodium.Stream.snapshot[B,C]* Stream.snapshot(Behavior,(A,B)=>C)]]
    * until the following transaction. To put this another way,
    * [[sodium.Stream.snapshot[B,C]* Stream.snapshot(Behavior,(A,B)=>C)]] always sees the value of a cell as it was before
    * any state changes from the current transaction.
    */
  final def hold(initValue: A): Cell[A] = new Cell[A](this.hold_(initValue))

  private def hold_(initValue: A): Behavior[A] =
    Transaction(_ => new Behavior[A](Stream.this, initValue))

  /**
    * A variant of [[sodium.Stream.hold(initValue:A)* hold(A)]] with an initial value captured by
    * [[Behavior.sampleLazy()* Behavior.sampleLazy()]].
    */
  def holdLazy(initValue: Lazy[A]): Cell[A] = Transaction(trans => new Cell[A](holdLazy_(trans, initValue)))

  private[sodium] def holdLazy_(trans: Transaction, initValue: Lazy[A]): Behavior[A] =
    Transaction(_ => new LazyBehavior[A](trans, this, Some(initValue)))

  //from C# implementation
  /**
    * Return a stream whose events are the values of the cell at the time of the stream event firing.
    */
  final def snapshot[B](c: Cell[B]): Stream[B] = snapshot(c.behavior)

  //from C# implementation
  /**
    * Return a stream whose events are the values of the behavior at the time of the stream event firing.
    */
  final def snapshot[B](b: Behavior[B]): Stream[B] = snapshot(b, (_: Any, a: B) => a)

  //from C# implementation
  /**
    * Return a stream whose events are the result of the combination using the specified
    * function of the input stream's value and the value of the cell at the time of the stream event firing.
    */
  final def snapshot[B, C](c: Cell[B], f: (A, B) => C): Stream[C] = snapshot(c.behavior, f)

  /**
    * Variant of [[sodium.Stream.snapshot[B,C]* snapshot(Behavior,(A,B)=>C)]] that captures the cell's value
    * at the time of the event firing, ignoring the stream's value.
    */
  //final def snapshot[B](c: Behavior[B]): Stream[B] = snapshot[B, B](c, (a, b) => b)

  /**
    * Return a stream whose events are the result of the combination using the specified
    * function of the input stream's event value and the value of the cell at that time.
    *
    * There is an implicit delay: State updates caused by event firings being held with
    * [[sodium.Stream.hold(initValue:A)* hold(A)]] don't become visible as the cell's current value until the
    * following transaction. To put this another way, [[sodium.Stream.snapshot[B,C]* snapshot(Behavior,(A,B)=>C)]]
    * always sees the value of a cell as it was before any state changes from the current
    * transaction.
    */
  final def snapshot[B, C](c: Behavior[B], f: (A, B) => C): Stream[C] = {
    val out = new StreamWithSend[C]()
    val l = listen_(out.node, (trans: Transaction, a: A) => {
      out.send(trans, f(a, c.sampleNoTrans()))
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Variant of [[sodium.Stream.snapshot[B,C]* snapshot(Behavior,(A,B)=>C)]] that captures the values of
    * two cells.
    */
  final def snapshot[B, C, D](cb: Cell[B], cc: Cell[C], fn: (A, B, C) => D): Stream[D] =
    this.snapshot[B, C, D](cb.behavior, cc.behavior, fn)

  final def snapshot[B, C, D](b1: Behavior[B], b2: Behavior[C], f: (A, B, C) => D): Stream[D] = {
    val out = new StreamWithSend[D]()
    val l = listen_(out.node, (trans: Transaction, a: A) => {
      out.send(trans, f(a, b1.sampleNoTrans(), b2.sampleNoTrans()))
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Variant of [[sodium.Stream.snapshot[B,C]* snapshot(Behavior,(A,B)=>C)]] that captures the values of
    * three cells.
    */
  final def snapshot[B, C, D, E](cb: Cell[B], cc: Cell[C], cd: Cell[D], fn: (A, B, C, D) => E): Stream[E] =
    this.snapshot[B, C, D, E](cb.behavior, cc.behavior, cd.behavior, fn)

  final def snapshot[B, C, D, E](b1: Behavior[B], b2: Behavior[C], b3: Behavior[D], f: (A, B, C, D) => E): Stream[E] = {
    val out = new StreamWithSend[E]()
    val l = listen_(out.node, (trans: Transaction, a: A) => {
      out.send(trans, f(a, b1.sampleNoTrans(), b2.sampleNoTrans(), b3.sampleNoTrans()))
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Variant of [[sodium.Stream.snapshot[B,C]* snapshot(Behavior,(A,B)=>C)]] that captures the values of
    * four cells.
    */
  final def snapshot[B, C, D, E, F](cb: Cell[B],
                                    cc: Cell[C],
                                    cd: Cell[D],
                                    ce: Cell[E],
                                    fn: (A, B, C, D, E) => F): Stream[F] =
    this.snapshot[B, C, D, E, F](cb.behavior, cc.behavior, cd.behavior, ce.behavior, fn)

  final def snapshot[B, C, D, E, F](b1: Behavior[B],
                                    b2: Behavior[C],
                                    b3: Behavior[D],
                                    b4: Behavior[E],
                                    f: (A, B, C, D, E) => F): Stream[F] = {
    val out = new StreamWithSend[F]()
    val l = listen_(out.node, (trans: Transaction, a: A) => {
      out.send(trans, f(a, b1.sampleNoTrans(), b2.sampleNoTrans(), b3.sampleNoTrans(), b4.sampleNoTrans()))
    })
    out.unsafeAddCleanup(l)
  }

  /**
    * Variant of [[sodium.Stream.snapshot[B,C]* snapshot(Behavior,(A,B)=>C)]] that captures the values of
    * five cells.
    */
  final def snapshot[B, C, D, E, F, G](cb: Behavior[B],
                                       cc: Behavior[C],
                                       cd: Behavior[D],
                                       ce: Behavior[E],
                                       cf: Behavior[F],
                                       fn: (A, B, C, D, E, F) => G): Stream[G] =
    this.snapshot[B, G](cb, (a: A, b: B) ⇒ fn(a, b, cc.sample(), cd.sample(), ce.sample(), cf.sample()))

  /**
    * Variant of [[Stream!.merge(s:sodium\.Stream[A],f:(A,A)=>A):sodium\.Stream[A]* merge(Stream,(A,A)=>A)]]
    * that merges two streams and will drop an event in the simultaneous case.
    *
    * In the case where two events are simultaneous (i.e. both
    * within the same transaction), the event from <em>this</em> will take precedence, and
    * the event from <em>s</em> will be dropped.
    * If you want to specify your own combining function,
    * use [[Stream!.merge(s:sodium\.Stream[A],f:(A,A)=>A):sodium\.Stream[A]* Stream.merge(Stream,(A,A)=>A)]].
    * s1.orElse(s2) is equivalent to s1.merge(s2, (l, r) -&gt; l).
    *
    * The name orElse() is used instead of merge() to make it really clear that care should
    * be taken, because events can be dropped.
    */
  final def orElse(s: Stream[A]): Stream[A] = merge(s, (left, _) => left)

  /**
    * Merge two streams of the same type into one, so that events on either input appear
    * on the returned stream.
    *
    * If the events are simultaneous (that is, one event from this and one from <em>s</em>
    * occurring in the same transaction), combine them into one using the specified combining function
    * so that the returned stream is guaranteed only ever to have one event per transaction.
    * The event from <em>this</em> will appear at the left input of the combining function, and
    * the event from <em>s</em> will appear at the right.
    *
    * @param f Function to combine the values. It may construct FRP logic or use
    *          [[sodium.Behavior.sample():A* Behavior.sample()]].
    *          Apart from this the function must be <em>referentially transparent</em>.
    */
  final def merge(s: Stream[A], f: ((A, A) => A)): Stream[A] = {
    Transaction(trans => Stream.merge(Stream.this, s).coalesce(trans, f))
  }

  final def coalesce(trans1: Transaction, f: (A, A) => A): Stream[A] = {
    val out = new StreamWithSend[A]()
    val h: TransactionHandler[A] = new CoalesceHandler[A](f, out)
    val l = listen(out.node, trans1, h, false)
    out.unsafeAddCleanup(l)
  }

  /**
    * Clean up the output by discarding any firing other than the last one.
    */
  final def lastFiringOnly(trans: Transaction): Stream[A] =
    coalesce(trans, (_, second) => second)

  /**
    * Return a stream that only outputs events for which the predicate returns true.
    */
  def filter(predicate: A => Boolean): Stream[A] = {
    val out = new StreamWithSend[A]()
    val l = listen_(out.node, (trans: Transaction, a: A) => if (predicate(a)) out.send(trans, a))
    out.unsafeAddCleanup(l)
  }

  /**
    * Return a stream that only outputs event occurrences from the input stream
    * when the specified cell's value is true.
    */
  final def gate(c: Behavior[Boolean]): Stream[A] =
    filterOptional(snapshot[Boolean, Option[A]](c, (a: A, pred: Boolean) => if (pred) Some(a) else None))

  final def gate(c: Cell[Boolean]): Stream[A] = gate(c.behavior)

  /**
    * Transform an event with a generalized state loop (a Mealy machine). The function
    * is passed the input and the old state and returns the new state and output value.
    *
    * @param f Function to apply to update the state. It may construct FRP logic or use
    *          [[sodium.Behavior.sample():A* Behavior.sample()]] in which case it is equivalent to
    *          [[sodium.Stream.snapshot[B]* Stream.snapshot(Behavior)]]ing the
    *          cell. Apart from this the function must be <em>referentially transparent</em>.
    */
  final def collect[B, S](initState: S, f: (A, S) => (B, S)): Stream[B] = collectLazy(new Lazy[S](initState), f)

  /**
    * A variant of [[sodium.Stream.collect[B,S](initState:S,f:(A,S)=>(B,S)):sodium\.Stream[B]* collect(S,(A,S)=>(B,S)]]
    * that takes an initial state returned by [[Behavior.sampleLazy()* Behavior.sampleLazy()]].
    */
  final def collectLazy[B, S](initState: Lazy[S], f: (A, S) => (B, S)): Stream[B] =
    Transaction(trans => {
      val es = new StreamLoop[S]()
      val s = es.holdLazy_(trans, initState)
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
    *          [[sodium.Behavior.sample():A* Behavior.sample()]] in which case it is equivalent to
    *          [[sodium.Stream.snapshot[B]* Stream.snapshot(Behavior)]]ing the
    *          cell. Apart from this the function must be <em>referentially transparent</em>.
    */
  final def accum[S](initState: S, f: (A, S) => S): Cell[S] = accumLazy(new Lazy[S](initState), f)

  /**
    * A variant of [[sodium.Stream.accum[S](initState:S,f:(A,S)=>S):sodium\.Behavior[S]* accum(S,(A,S)=>S)]] that takes an
    *  initial state returned by [[Behavior.sampleLazy()* Behavior.sampleLazy()]].
    */
  final def accumLazy[S](initState: Lazy[S], f: (A, S) => S): Cell[S] =
    Transaction(trans => {
      val es = new StreamLoop[S]()
      val s = es.holdLazy_(trans, initState)
      val es_out = snapshot(s, f)
      es.loop(es_out)
      es_out.holdLazy(initState)
    })

  /**
    * Return a stream that outputs only one value: the next event occurrence of the
    * input stream, starting from the transaction in which once() was invoked.
    */
  final def once(): Stream[A] = {
    // This is a bit long-winded but it's efficient because it deregisters
    // the listener.
    val ev = this
    var la: Option[Listener] = None
    val out = new StreamWithSend[A]()
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
    *
    * You must use this only with listeners returned by [[listenWeak* listenWeak(A=>Unit)]] so that
    * things don't get kept alive when they shouldn't.
    */
  def addCleanup(cleanup: Listener): Stream[A] =
    Transaction(_ => {
      val fsNew: ListBuffer[Listener] = finalizers
      fsNew += cleanup
      new Stream[A](node, fsNew, firings)
    })

  protected override def finalize(): Unit = {
    finalizers.foreach(_.unlisten())
  }
}

object Stream {

  val keepListenersAlive = new mutable.HashSet[Listener]()

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
  private def merge[A](ea: Stream[A], eb: Stream[A]): Stream[A] = {
    val out = new StreamWithSend[A]()
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
    * Variant of [[sodium.Stream.orElse(s:sodium\.Stream[A]):sodium\.Stream[A]* orElse(Stream)]]
    * that merges a collection of streams.
    */
  def orElse[A](ss: Iterable[Stream[A]]): Stream[A] = Stream.merge[A](ss, (_: A, right: A) => right)

  /**
    * Variant of [[Stream!.merge(s:sodium\.Stream[A],f:(A,A)=>A):sodium\.Stream[A]* Stream.merge(Stream,(A,A)=>A)]]
    * that merges a collection of streams.
    */
  def merge[A](ss: Iterable[Stream[A]], f: (A, A) => A): Stream[A] = {
    val ss_ : Array[Stream[A]] = ss.toArray
    Stream.merge[A](ss_, 0, ss_.length, f)
  }

  //TODO this method is recursive but not tailrec
  private def merge[A](sas: Array[Stream[A]], start: Int, end: Int, f: (A, A) => A): Stream[A] = {
    val len = end - start
    len match {
      case 0 => new Stream[A]()
      case 1 => sas(start)
      case 2 => sas(start).merge(sas(start + 1), f)
      case _ =>
        val mid = (start + end) / 2
        Stream.merge[A](sas, start, mid, f).merge(Stream.merge[A](sas, mid, end, f), f)
    }
  }

  /**
    * Return a stream that only outputs events that have present
    * values, removing the scala.Option wrapper, discarding empty values.
    */
  def filterOptional[A](ev: Stream[Option[A]]): Stream[A] = {
    val out = new StreamWithSend[A]()
    val l = ev.listen_(out.node, (trans: Transaction, oa: Option[A]) => {
      oa.foreach(out.send(trans, _))
    })
    out.unsafeAddCleanup(l)
  }

}
