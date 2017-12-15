package sodium

import sodium.Node.Target

class Cell[A](protected var currentValue: Option[A], final protected val event: Stream[A]) {
  import Cell._

  private var valueUpdate: Option[A] = None
  private var cleanup: Option[Listener] = None
  protected var lazyInitValue: Option[() => A] = None // Used by LazyCell

  Transaction({ trans1 =>
    cleanup = Some(
      event.listen(
        Node.NullNode,
        trans1,
        new TransactionHandler[A]() {
          def run(trans2: Transaction, a: A): Unit = {
            if (Cell.this.valueUpdate.isEmpty) {
              trans2.last(new Runnable() {
                def run(): Unit = {
                  Cell.this.currentValue = Cell.this.valueUpdate
                  Cell.this.lazyInitValue = None
                  Cell.this.valueUpdate = None
                }
              })
            }
            valueUpdate = Some(a)
          }
        },
        false
      ))
  })

  def this(initValue: A, event: Stream[A]) {
    this(Some(initValue), event)
  }

  def this(initValue: A) {
    this(Some(initValue), new Stream[A]())
  }

  /**
    * @return The value including any updates that have happened in this transaction.
    */
  final def newValue(): A = valueUpdate.getOrElse(sampleNoTrans)

  /**
    * Sample the behavior's current value.
    *
    * This should generally be avoided in favour of value().listen(..) so you don't
    * miss any updates, but in many circumstances it makes sense.
    *
    * It can be best to use it inside an explicit transaction (using Transaction.run()).
    * For example, a b.sample() inside an explicit transaction along with a
    * b.updates().listen(..) will capture the current value and any updates without risk
    * of missing any in between.
    */
  final def sample(): A = Transaction(_ => sampleNoTrans())

  def sampleNoTrans(): A = currentValue.get

  /**
    * An event that gives the updates for the behavior. If this behavior was created
    * with a hold, then updates() gives you an event equivalent to the one that was held.
    */
  final def updates(): Stream[A] = event

  /**
    * An event that is guaranteed to fire once when you listen to it, giving
    * the current value of the behavior, and thereafter behaves like updates(),
    * firing for each update to the behavior's value.
    */
  final def value(): Stream[A] = Transaction(trans => value(trans))

  final def value(trans1: Transaction): Stream[A] = {
    val sSpark = new StreamSink[Unit]()
    trans1.prioritized(sSpark.node, trans2 => sSpark.send(trans2, ()))

    val sInitial = sSpark.snapshot[A](this)
    sInitial.merge(updates()).lastFiringOnly(trans1)
  }

  /**
    * Transform the behavior's value according to the supplied function.
    */
  final def map[B](f: A => B): Cell[B] =
    updates().map(f).holdLazy(() => f(sampleNoTrans()))

  /**
    * Lift a binary function into behaviors.
    */
  final def lift[B, C](f: (A, B) => C, b: Cell[B]): Cell[C] = {
    def ffa(aa: A)(bb: B) = f(aa, bb)
    apply(map(ffa), b)
  }

  /**
    * Lift a ternary function into behaviors.
    */
  final def lift[B, C, D](f: (A, B, C) => D, b: Cell[B], c: Cell[C]): Cell[D] = {
    def ffa(aa: A)(bb: B)(cc: C) = f(aa, bb, cc)
    apply(apply(map(ffa), b), c)
  }

  /**
    * Lift a quaternary function into behaviors.
    */
  final def lift[B, C, D, E](f: (A, B, C, D) => E, b: Cell[B], c: Cell[C], d: Cell[D]): Cell[E] = {
    def ffa(aa: A)(bb: B)(cc: C)(dd: D) = f(aa, bb, cc, dd)
    apply(apply(apply(map(ffa), b), c), d)
  }

  /**
    * Transform a behavior with a generalized state loop (a mealy machine). The function
    * is passed the input and the old state and returns the new state and output value.
    */
  final def collect[B, S](initState: S, f: (A, S) => (B, S)): Cell[B] =
    Transaction[Cell[B]](_ => {
      val ea = updates().coalesce((fst, snd) => snd)
      val ebs = new StreamLoop[(B, S)]()
      val bbs = ebs.holdLazy(() => f(sampleNoTrans(), initState))
      val bs = bbs.map(x => x._2)
      val ebs_out = ea.snapshot(bs, f)
      ebs.loop(ebs_out)
      bbs.map(x => x._1)
    })

  protected override def finalize(): Unit = {
    cleanup.foreach(_.unlisten())
  }

  /**
    * Listen for firings of this event. The returned Listener has an unlisten()
    * method to cause the listener to be removed. This is the observer pattern.
    */
  final def listen(action: A => Unit): Listener = {
    Transaction(trans => value.listen(action))
  }

}

object Cell {

  /**
    * Lift a binary function into behaviors.
    */
  final def lift[A, B, C](f: (A, B) => C, a: Cell[A], b: Cell[B]): Cell[C] =
    a.lift(f, b)

  /**
    * Lift a ternary function into behaviors.
    */
  final def lift[A, B, C, D](f: (A, B, C) => D, a: Cell[A], b: Cell[B], c: Cell[C]): Cell[D] =
    a.lift(f, b, c)

  /**
    * Lift a quaternary function into behaviors.
    */
  final def lift[A, B, C, D, E](f: (A, B, C, D) => E, a: Cell[A], b: Cell[B], c: Cell[C], d: Cell[D]): Cell[E] =
    a.lift(f, b, c, d)

  /**
    * Apply a value inside a behavior to a function inside a behavior. This is the
    * primitive for all function lifting.
    */
  def apply[A, B](bf: Cell[A => B], ba: Cell[A]): Cell[B] =
    Transaction(trans0 => {

      val out = new StreamSink[B]
      final class ApplyHandler(val trans0: Transaction) {
        resetFired(trans0) // We suppress firing during the first transaction

        var fired = true
        var a: A = _
        var f: A => B = _

        def run(trans1: Transaction): Unit = {
          if (fired) ()
          else {
            fired = true
            trans1.prioritized(out.node, { trans2 =>
              out.send(trans2, f(a))
            })
          }

          resetFired(trans1)
        }

        def resetFired(trans1: Transaction): Unit = {
          trans1.last(() => {
            fired = false
          })
        }
      }
      val out_target = out.node
      val in_target = new Node(0)
      val node_target_ = new Array[Target](1)
      in_target.linkTo(null, out_target, node_target_)
      val node_target: Node.Target = node_target_(0)
      val h: ApplyHandler = new ApplyHandler(trans0)
      val l1 = bf
        .value()
        .listen_(in_target, new TransactionHandler[A => B]() {
          def run(trans1: Transaction, f: A => B): Unit = {
            h.f = f
            h.run(trans1)
          }
        })

      val l2 = ba
        .value()
        .listen_(in_target, new TransactionHandler[A]() {
          def run(trans1: Transaction, a: A): Unit = {
            h.a = a
            h.run(trans1)
          }
        })
      out
        .unsafeAddCleanup(l1)
        .unsafeAddCleanup(l2)
        .unsafeAddCleanup(new Listener() {
          def unlisten(): Unit = {
            in_target.unlinkTo(node_target)
          }
        })
        .holdLazy(() => bf.sampleNoTrans().apply(ba.sampleNoTrans()))

    })

  /**
    * Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
    */
  def switchC[A](bba: Cell[Cell[A]]): Cell[A] = {
    Transaction(trans0 => {
      def za = () => bba.sampleNoTrans().sampleNoTrans

      val out = new StreamSink[A]()
      val h = new TransactionHandler[Cell[A]]() {
        private var currentListener: Option[Listener] = None

        override def run(trans: Transaction, ba: Cell[A]): Unit = {
          // Note: If any switch takes place during a transaction, then the
          // value().listen will always cause a sample to be fetched from the
          // one we just switched to. The caller will be fetching our output
          // using value().listen, and value() throws away all firings except
          // for the last one. Therefore, anything from the old input behaviour
          // that might have happened during this transaction will be suppressed.
          currentListener.foreach(_.unlisten())
          currentListener = Some(
            ba.value(trans)
              .listen(out.node, trans, new TransactionHandler[A]() {
                def run(trans3: Transaction, a: A): Unit = {
                  out.send(trans3, a)
                }
              }, false))
        }

        override def finalize(): Unit = {
          currentListener.foreach(_.unlisten())
        }
      }
      val l = bba.value().listen_(out.node, h)
      out.unsafeAddCleanup(l).holdLazy(za)
    })
  }

  /**
    * Unwrap an event inside a behavior to give a time-varying event implementation.
    */
  def switchS[A](bea: Cell[Stream[A]]): Stream[A] = {
    def switchS(trans1: Transaction, bea: Cell[Stream[A]]): Stream[A] = {
      val out = new StreamSink[A]()
      val h2 = new TransactionHandler[A]() {
        def run(trans2: Transaction, a: A): Unit = {
          out.send(trans2, a)
        }
      }
      val h1 = new TransactionHandler[Stream[A]]() {
        private var currentListener = bea.sampleNoTrans().listen(out.node, trans1, h2, false)

        override def run(trans2: Transaction, ea: Stream[A]): Unit = {
          trans2.last(new Runnable() {
            def run(): Unit = {
              currentListener.unlisten()
              currentListener = ea.listen(out.node, trans2, h2, true)
            }
          })
        }

        override def finalize(): Unit = {
          currentListener.unlisten()
        }
      }
      val l1 = bea.updates().listen(out.node, trans1, h1, false)
      out.unsafeAddCleanup(l1)
    }

    Transaction(trans => switchS(trans, bea))
  }

}
