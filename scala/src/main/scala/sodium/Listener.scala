package sodium

/**
  * A handle for a listener that was registered with [[Cell.listen Cell.listen(A=>Unit)]] or
  * [[Stream.listen(handler:A=>Unit):sodium\.Listener* Stream.listen(A=>Unit)]].
  * To keep the FRP logic alive, you must prevent this object being garbage
  * collected. You're recommended either to ensure you call [[sodium.Listener.unlisten()* unlisten()]] when you've
  * finished with it, or to store it in a field of an object that you know will stay in memory.
  */
abstract class Listener {

  /**
    * Deregister the listener that was registered so it will no longer be called back,
    * allowing associated resources to be garbage collected.
    */
  def unlisten(): Unit

  /**
    * Combine listeners into one so that invoking [[sodium.Listener.unlisten()* unlisten()]] on the returned
    * listener will unlisten both the inputs.
    */
  final def append(two: Listener): Listener = {
    val one = this
    new Listener() {
      override def unlisten(): Unit = {
        one.unlisten()
        two.unlisten()
      }
    }
  }
}
