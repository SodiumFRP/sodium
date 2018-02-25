package sodium

/**
  * A forward reference for a [[Cell]] equivalent to the Cell that is referenced.
  */
final class CellLoop[A](behaviorLoop: BehaviorLoop[A]) extends Cell[A](behaviorLoop) {

  val streamLoop = new StreamLoop[A]()

  def this() = this(new BehaviorLoop[A]())

  /**
    * Resolve the loop to specify what the CellLoop was a forward reference to. It
    * must be invoked inside the same transaction as the place where the CellLoop is used.
    * This requires you to create an explicit transaction with [[Transaction]].
    */
  def loop(c: Cell[A]): Unit = {
    streamLoop.loop(c.updates())
    behaviorLoop.loop(c.behavior)
    ()
  }

}
