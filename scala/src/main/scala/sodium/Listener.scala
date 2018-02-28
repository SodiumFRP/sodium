package sodium

/**
  * A handle for a listener that was registered with [[Behavior.listen Behavior.listen(A=>Unit)]] or
  * [[Stream.listen(handler:A=>Unit):sodium\.Listener* Stream.listen(A=>Unit)]].
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
    () =>
      {
        one.unlisten()
        two.unlisten()
      }
  }
}
