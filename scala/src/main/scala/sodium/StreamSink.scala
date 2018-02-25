package sodium

/**
  * A stream that allows values to be pushed into it, acting as an interface between the
  * world of I/O and the world of FRP. Code that exports StreamSinks for read-only use
  * should downcast to [[Stream]].
  *
  *  @constructor If you send more than one event in a transaction, they are combined into a
  * single event using the specified function. The combining function should be
  * <em>associative</em>.
  * @param f Function to combine the values. It may construct FRP logic or use
  *          [[sodium.Behavior.sample():A* Behavior.sample()]].
  *          Apart from this the function must be <em>referentially transparent</em>.
  */
class StreamSink[A](val f: (A, A) => A) extends StreamWithSend[A] {

  private val coalescer: CoalesceHandler[A] = new CoalesceHandler[A](f, this)

  //TODO Scaladoc bug [[sodium.StreamSink(f:(A,A)=>A):sodium\.StreamSink[A]* StreamSink((A,A)=>A)]]
  /**
    * Construct a StreamSink that allows send() to be called once on it per transaction.
    * If you call send() more than once, it will throw an exception. If you need to do
    * this, then use [[StreamSink StreamSink((A,A)=>A)]].
    */
  def this() =
    this(
      (_: A, _: A) =>
        throw new RuntimeException(
          """send() called more than once per transaction, which isn't allowed. Did you want to combine the events?
            |Then pass a combining function to your StreamSink constructor.""".stripMargin))

  /**
    * Send a value to be made available to consumers of the stream. send(A) may not be used inside
    * handlers registered with [[Stream.listen(handler:A=>Unit):sodium\.Listener* Stream.listen(A=>Unit)]] or
    * [[Behavior.listen Behavior.listen(A=>Unit)]].
    * An exception will be thrown, because StreamSink is for interfacing I/O to FRP only.
    * You are not meant to use this to define your own primitives.
    *
    * @param a Value to push into the cell.
    */
  def send(a: A): Unit = {
    Transaction(trans => {
      if (Transaction.inCallback > 0) {
        throw new RuntimeException("You are not allowed to use send() inside a Sodium callback")
      }
      coalescer.run(trans, a)
    })
  }
}
