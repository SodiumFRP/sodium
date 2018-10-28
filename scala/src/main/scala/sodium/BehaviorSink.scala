package sodium

/**
  * A cell that allows values to be pushed into it, acting as an interface between the
  * world of I/O and the world of FRP. Code that exports CellSinks for read-only use
  * should downcast to [[Behavior]].
  *
  * @constructor Construct a writable cell with the specified initial value. If multiple values are
  * sent in the same transaction, the specified function is used to combine them.
  */
final class BehaviorSink[A](initValue: A, f: (A, A) => A) extends Behavior[A](new StreamSink[A](f), Some(initValue)) {

  /**
    * Construct a writable cell with the specified initial value.
    * If multiple values are sent in the same transaction, the last one is used.
    */
  //TODO remove this trick, what we really want is the constructor StreamSink.this() not a copy of it.
  def this(initValue: A) =
    this(
      initValue,
      (_: A, _: A) =>
        throw new RuntimeException(
          """send() called more than once per transaction, which isn't allowed. Did you want to combine the events?
        |Then pass a combining function to your StreamSink constructor.""".stripMargin)
    )

  /**
    * Send a value, modifying the value of the cell. send(A) may not be used inside
    * handlers registered with [[Stream.listen(handler:A=>Unit):sodium\.Listener* Stream.listen(A=>Unit)]] or
    * [[Behavior.listen Behavior.listen(A=>Unit)]].
    * An exception will be thrown, because BehaviorSink is for interfacing I/O to FRP only.
    * You are not meant to use this to define your own primitives.
    *
    * @param a Value to push into the cell.
    */
  def send(a: A): Unit = {
    str match {
      case s: StreamSink[A] => s.send(a)
      case _                =>
    }
  }
}
