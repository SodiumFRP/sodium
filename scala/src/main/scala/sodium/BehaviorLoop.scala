package sodium

/**
  * A forward reference for a [[Behavior]] equivalent to the Behavior that is referenced.
  */
final class BehaviorLoop[A](streamLoop: StreamLoop[A]) extends LazyBehavior[A](streamLoop, None) {

  def this() = this(new StreamLoop[A]())

  /**
    * Resolve the loop to specify what the BehaviorLoop was a forward reference to. It
    * must be invoked inside the same transaction as the place where the BehaviorLoop is used.
    * This requires you to create an explicit transaction with [[Transaction]].
    */
  def loop(b: Behavior[A]): Unit = {
    Transaction(trans => {
      streamLoop.loop(b.updates())
      lazyInitValue = Some(b.sampleLazy(trans))
      ()
    })
  }

  override def sampleNoTrans(): A = {
    str match {
      case s: StreamLoop[A] => if (!s.assigned) throw new RuntimeException("BehaviorLoop sampled before it was looped")
      case _                =>
    }
    super.sampleNoTrans()
  }
}
