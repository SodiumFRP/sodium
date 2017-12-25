package sodium

/**
  * A stream that allows values to be pushed into it, acting as an interface between the
  * world of I/O and the world of FRP. Code that exports StreamSinks for read-only use
  * should downcast to [[Stream]].
  *  @constructor If you send more than one event in a transaction, they are combined into a
  * single event using the specified function. The combining function should be
  * <em>associative</em>.
  * @param f Function to combine the values. It may construct FRP logic or use
  *  [[sodium.Cell.sample():A* Cell.sample()]].
  *  Apart from this the function must be <em>referentially transparent</em>.
  */
class StreamSink[A](val f: (A, A) => A) extends StreamWithSend[A] {

  private val coalescer: CoalesceHandler[A] = new CoalesceHandler[A](f, this)

  def this() = this((left: A, right: A) => right)

  /**
    * Send a value to be made available to consumers of the stream. send(A) may not be used inside
    * handlers registered with [[Stream.listen(handler:A=>Unit):sodium\.Listener* Stream.listen(A=>Unit)]] or
    * [[Cell.listen Cell.listen(A=>Unit)]].
    * An exception will be thrown, because StreamSink is for interfacing I/O to FRP only.
    * You are not meant to use this to define your own primitives.
    *
    * @param a Value to push into the cell.
    */
  def send(a: A): Unit = {
    Transaction(trans => {
      if (Transaction.inCallback > 0)
        throw new RuntimeException("You are not allowed to use send() inside a Sodium callback")
      coalescer.run(trans, a)
    })
  }
}
