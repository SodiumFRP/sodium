package sodium

/**
  * A stream that allows values to be pushed into it, acting as an interface between the
  * world of I/O and the world of FRP. Code that exports StreamSinks for read-only use
  * should downcast to [[Stream]].
  */
class StreamSink[A] extends StreamWithSend[A] {

  /**
    * Send a value to be made available to consumers of the stream. send(A) may not be used inside
    * handlers registered with [[Stream.listen(action:A=>Unit):sodium\.Listener* Stream.listen(A=>Unit)]] or
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
      send(trans, a)
    })
  }
}
