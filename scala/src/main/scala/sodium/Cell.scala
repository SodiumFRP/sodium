package sodium

/**
  * Represents a value of type A that discretely changes over time.
  */
class Cell[A](val behavior: Behavior[A]) {

  private val cleanup: Option[Listener] = None
  var lazyInitValue: Option[Lazy[A]] = None // Used by LazyCell

  /**
    * A cell with a constant value.
    */
  def this(initValue: A) {
    this(new Behavior[A](new Stream[A](), Some(initValue)))
  }

  def this(event: Stream[A], initValue: A) {
    this(new Behavior[A](event, Some(initValue)))
  }

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
  final def sample(): A = behavior.sample()

  /**
    * A variant of [[sample():A* sample()]] that works with [[CellLoop]]s when they haven't been looped yet.
    * It should be used in any code that's general enough that it could be passed a [[CellLoop]].
    *
    * @see [[sodium.Stream!.holdLazy(initValue:sodium\.Lazy[A]):sodium\.Cell[A]* Stream!.holdLazy()]]
    */
  final def sampleLazy(): Lazy[A] = behavior.sampleLazy()

  final def updates(): Stream[A] = Transaction(trans => behavior.updates().coalesce(trans, (_, r) => r))

  final def values() = Transaction(trans => behavior.value(trans))

  /**
    * Transform the cell's value according to the supplied function, so the returned Cell
    * always reflects the value of the function applied to the input Cell's value.
    *
    * @param f Function to apply to convert the values. It must be <em>referentially transparent</em>.
    */
  final def map[B](f: A => B): Cell[B] = new Cell(behavior.map(f))

  /**
    * Lift a binary function into cells, so the returned Cell always reflects the specified
    * function applied to the input cells' values.
    *
    * @param fn Function to apply. It must be <em>referentially transparent</em>.
    */
  final def lift[B, C](b: Cell[B], fn: (A, B) => C): Cell[C] = new Cell(behavior.lift(b.behavior, fn))

  /**
    * Lift a ternary function into cells, so the returned Cell always reflects the specified
    * function applied to the input cells' values.
    *
    * @param fn Function to apply. It must be <em>referentially transparent</em>.
    */
  final def lift[B, C, D](b: Cell[B], c: Cell[C], fn: (A, B, C) => D): Cell[D] =
    new Cell(behavior.lift(b.behavior, c.behavior, fn))

  /**
    * Lift a quaternary function into cells, so the returned Cell always reflects the specified
    * function applied to the input cells' values.
    *
    * @param fn Function to apply. It must be <em>referentially transparent</em>.
    */
  final def lift[B, C, D, E](b: Cell[B], c: Cell[C], d: Cell[D], fn: (A, B, C, D) => E): Cell[E] =
    new Cell(behavior.lift(b.behavior, c.behavior, d.behavior, fn))

  /**
    * Lift a 5-argument function into cells, so the returned Cell always reflects the specified
    * function applied to the input cells' values.
    *
    * @param fn Function to apply. It must be <em>referentially transparent</em>.
    */
  final def lift[B, C, D, E, F](b: Cell[B], c: Cell[C], d: Cell[D], e: Cell[E], fn: (A, B, C, D, E) => F): Cell[F] =
    new Cell(behavior.lift(b.behavior, c.behavior, d.behavior, e.behavior, fn))

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
                                   fn: (A, B, C, D, E, F) => G): Cell[G] =
    new Cell(behavior.lift(b.behavior, c.behavior, d.behavior, e.behavior, f.behavior, fn))

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
  final def listen(action: A => Unit): Listener = Transaction(trans => behavior.value(trans).listen(action))

  /**
    * A variant of [[sodium.Cell.listen(action:A=>Unit):sodium\.Listener* listen(A=>Unit)]] that will deregister
    * the listener automatically if the listener is garbage collected. With
    * [[sodium.Cell.listen(action:A=>Unit):sodium\.Listener* listen(A=>Unit)]], the listener is
    * only deregistered if [[sodium.Listener.unlisten()* Listener.unlisten()]] is called explicitly.
    */
  final def listenWeak(action: A => Unit): Listener = Transaction(trans => behavior.value(trans).listenWeak(action))

}

object Cell {

  def switchC[A](cca: Cell[Cell[A]]): Cell[A] = new Cell(Behavior.switchC(cca.behavior.map(_.behavior)))

  def switchS[A](csa: Cell[Stream[A]]): Stream[A] = Behavior.switchS(csa.behavior)

}
