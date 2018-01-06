package sodium

import sodium.Node.Target

/**
  * Represents a value of type A that changes over time.
  */
class Cell[A](val str: Stream[A], protected var currentValue: Option[A]) {

  private var valueUpdate: Option[A] = None
  private var cleanup: Option[Listener] = None
  var lazyInitValue: Option[Lazy[A]] = None // Used by LazyCell

  Transaction({ trans1 =>
    cleanup = Some(
      str.listen(
        Node.NullNode,
        trans1,
        (trans2: Transaction, a: A) => {
          if (Cell.this.valueUpdate.isEmpty) {
            trans2.last(() => {
              Cell.this.currentValue = Cell.this.valueUpdate
              Cell.this.lazyInitValue = None
              Cell.this.valueUpdate = None
            })
          }
          valueUpdate = Some(a)
        },
        false
      ))
  })

  /**
    * A cell with a constant value.
    */
  def this(initValue: A) {
    this(new Stream[A](), Some(initValue))
  }

  def this(event: Stream[A], initValue: A) {
    this(event, Some(initValue))
  }

  /**
    * @return The value including any updates that have happened in this transaction.
    */
  final def newValue(): A = valueUpdate.getOrElse(sampleNoTrans)

  /**
    * Sample the cell's current value.
    *
    * It may be used inside the functions passed to primitives that apply them to [[Stream]]s,
    * including [[Stream.map Stream.map(A=>B)]]] in which case it is equivalent to snapshotting the cell,
    * [[sodium.Stream.snapshot[B,C]* Stream.snapshot(Cell,(A,B)=>C)]], [[Stream.filter Stream.filter(A=>Boolean)]] and
    * [[Stream!.merge(s:sodium\.Stream[A],f:(A,A)=>A):sodium\.Stream[A]* Stream.merge(Stream,(A,A)=>A)]]
    * It should generally be avoided in favour of [[listen listen(A=>Unit)]] so you don't
    * miss any updates, but in many circumstances it makes sense.
    */
  final def sample(): A = Transaction(_ => sampleNoTrans())

  /**
    * A variant of [[sample():A* sample()]] that works with [[CellLoop]]s when they haven't been looped yet.
    * It should be used in any code that's general enough that it could be passed a [[CellLoop]].
    *
    * @see [[sodium.Stream!.holdLazy(initValue:sodium\.Lazy[A]):sodium\.Cell[A]* Stream!.holdLazy()]]
    */
  final def sampleLazy(): Lazy[A] = {
    val me = this
    Transaction(trans => me.sampleLazy(trans))
  }

  final def sampleLazy(trans: Transaction): Lazy[A] = {
    val me = this
    val s = new Cell.LazySample[A](me)
    trans.last(() => {
      s.value = me.valueUpdate.getOrElse(me.sampleNoTrans)
      s.hasValue = true
      s.cell = null
    })
    new Lazy[A](
      () =>
        if (s.hasValue) s.value
        else s.cell.sample)
  }

  def sampleNoTrans(): A = currentValue.get

  final def updates(): Stream[A] = str

  final def value(trans1: Transaction): Stream[A] = {
    val sSpark = new StreamWithSend[Unit]()
    trans1.prioritized(sSpark.node, trans2 => sSpark.send(trans2, ()))
    val sInitial = sSpark.snapshot[A](this)
    sInitial.merge(updates(), (left, right) => right)
  }

  /**
    * Transform the cell's value according to the supplied function, so the returned Cell
    * always reflects the value of the function applied to the input Cell's value.
    *
    * @param f Function to apply to convert the values. It must be <em>referentially transparent</em>.
    */
  final def map[B](f: A => B) =
    Transaction(trans => updates().map(f).holdLazy(sampleLazy(trans).map(f)))

  /**
    * Lift a binary function into cells, so the returned Cell always reflects the specified
    * function applied to the input cells' values.
    *
    * @param fn Function to apply. It must be <em>referentially transparent</em>.
    */
  final def lift[B, C](b: Cell[B], fn: (A, B) => C): Cell[C] = {
    def ffa(aa: A)(bb: B) = fn(aa, bb)
    val bf: Cell[B => C] = this.map(ffa)
    Cell(bf, b)
  }

  /**
    * Lift a ternary function into cells, so the returned Cell always reflects the specified
    * function applied to the input cells' values.
    *
    * @param fn Function to apply. It must be <em>referentially transparent</em>.
    */
  final def lift[B, C, D](b: Cell[B], c: Cell[C], fn: (A, B, C) => D): Cell[D] = {
    def ffa(aa: A)(bb: B)(cc: C) = fn(aa, bb, cc)
    Cell(Cell(this.map(ffa), b), c)
  }

  /**
    * Lift a quaternary function into cells, so the returned Cell always reflects the specified
    * function applied to the input cells' values.
    *
    * @param fn Function to apply. It must be <em>referentially transparent</em>.
    */
  final def lift[B, C, D, E](b: Cell[B], c: Cell[C], d: Cell[D], fn: (A, B, C, D) => E): Cell[E] = {
    def ffa(aa: A)(bb: B)(cc: C)(dd: D) = fn(aa, bb, cc, dd)
    Cell(Cell(Cell(this.map(ffa), b), c), d)
  }

  /**
    * Lift a 5-argument function into cells, so the returned Cell always reflects the specified
    * function applied to the input cells' values.
    *
    * @param fn Function to apply. It must be <em>referentially transparent</em>.
    */
  final def lift[B, C, D, E, F](b: Cell[B], c: Cell[C], d: Cell[D], e: Cell[E], fn: (A, B, C, D, E) => F): Cell[F] = {
    def ffa(aa: A)(bb: B)(cc: C)(dd: D)(ee: E) = fn(aa, bb, cc, dd, ee)
    Cell(Cell(Cell(Cell(this.map(ffa), b), c), d), e)
  }

  /**
    * Lift a 6-argument function into cells, so the returned Cell always reflects the specified
    * function applied to the input cells' values.
    *
    * @param fn Function to apply. It must be <em>referentially transparent</em>.
    */
  final def lift[B, C, D, E, F, G](b: Cell[B],
                                   c: Cell[C],
                                   d: Cell[D],
                                   e: Cell[E],
                                   f: Cell[F],
                                   fn: (A, B, C, D, E, F) => G): Cell[G] = {
    def ffa(aa: A)(bb: B)(cc: C)(dd: D)(ee: E)(ff: F) = fn(aa, bb, cc, dd, ee, ff)
    Cell(Cell(Cell(Cell(Cell(this.map(ffa), b), c), d), e), f)
  }

  protected override def finalize(): Unit = {
    cleanup.foreach(_.unlisten())
  }

  /**
    * Listen for updates to the value of this cell. This is the observer pattern. The
    * returned [[Listener]] has a [[sodium.Listener.unlisten()* Listener.unlisten()]] method to cause the
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
  final def listen(action: A => Unit) = Transaction(trans => value(trans).listen(action))

  /**
    * A variant of [[sodium.Cell.listen(action:A=>Unit):sodium\.Listener* listen(A=>Unit)]] that will deregister
    * the listener automatically if the listener is garbage collected. With
    * [[sodium.Cell.listen(action:A=>Unit):sodium\.Listener* listen(A=>Unit)]], the listener is
    * only deregistered if [[sodium.Listener.unlisten()* Listener.unlisten()]] is called explicitly.
    */
  final def listenWeak(action: A => Unit) = Transaction(trans => value(trans).listenWeak(action))

}

object Cell {

  private class LazySample[A] private[sodium] (var cell: Cell[A]) {
    private[sodium] var hasValue = false
    private[sodium] var value: A = _
  }

  /**
    * Apply a value inside a cell to a function inside a cell. This is the
    * primitive for all function lifting.
    */
  def apply[A, B](bf: Cell[A => B], ba: Cell[A]): Cell[B] =
    Transaction(trans0 => {
      val out = new StreamWithSend[B]()

      final class ApplyHandler(val trans0: Transaction) {
        var a: A = _
        var f_present = false
        var f: A => B = _
        var a_present = false
        def run(trans1: Transaction): Unit = {
          trans1.prioritized(out.node, { trans2 =>
            out.send(trans2, f(a))
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
        .value(trans0)
        .listen_(in_target, (trans1: Transaction, f: A => B) => {
          h.f = f
          h.f_present = true
          if (h.a_present)
            h.run(trans1)
        })

      val l2 = ba
        .value(trans0)
        .listen_(in_target, (trans1: Transaction, a: A) => {
          h.a = a
          h.a_present = true
          if (h.f_present)
            h.run(trans1)
        })
      out
        .lastFiringOnly(trans0)
        .unsafeAddCleanup(l1)
        .unsafeAddCleanup(l2)
        .unsafeAddCleanup(new Listener() {
          def unlisten(): Unit = {
            in_target.unlinkTo(node_target)
          }
        })
        .holdLazy(new Lazy(() => bf.sampleNoTrans().apply(ba.sampleNoTrans())))

    })

  /**
    * Unwrap a stream inside another behavior to give a time-varying stream implementation.
    */
  def switchC[A](bba: Cell[Cell[A]]): Cell[A] =
    Transaction(trans0 => {
      val za = bba.sampleLazy.map((ba: Cell[A]) => ba.sample)

      val out = new StreamWithSend[A]()
      val h = new TransactionHandler[Cell[A]]() {
        private var currentListener: Option[Listener] = None

        override def run(trans: Transaction, ba: Cell[A]): Unit = {
          // Note: If any switch takes place during a transaction, then the
          // lastFiringOnly() below will always cause a sample to be fetched
          // from the one we just switched to. So anything from the old input cell
          // that might have happened during this transaction will be suppressed.
          currentListener.foreach(_.unlisten())
          currentListener = Some(
            ba.value(trans)
              .listen(out.node, trans, (trans3: Transaction, a: A) => {
                out.send(trans3, a)
              }, false))
        }

        override def finalize(): Unit = {
          currentListener.foreach(_.unlisten())
        }
      }
      val l = bba.value(trans0).listen_(out.node, h)
      out.lastFiringOnly(trans0).unsafeAddCleanup(l).holdLazy(za)
    })

  /**
    * Unwrap an event inside a cell to give a time-varying event implementation.
    */
  def switchS[A](bea: Cell[Stream[A]]): Stream[A] = {
    def switchS(trans1: Transaction, bea: Cell[Stream[A]]): Stream[A] = {
      val out = new StreamWithSend[A]()
      val h2 = new TransactionHandler[A]() {
        def run(trans2: Transaction, a: A): Unit = {
          out.send(trans2, a)
        }
      }
      val h1 = new TransactionHandler[Stream[A]]() {
        private var currentListener = bea.sampleNoTrans().listen(out.node, trans1, h2, false)

        override def run(trans2: Transaction, ea: Stream[A]): Unit = {
          trans2.last(() => {
            currentListener.unlisten()
            currentListener = ea.listen(out.node, trans2, h2, true)
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
