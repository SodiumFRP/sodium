package sodium

/**
  * A forward reference for a [[Cell]] equivalent to the Cell that is referenced.
  */
final class CellLoop[A] extends LazyCell[A](new StreamLoop[A](), None) {

  /**
    * Resolve the loop to specify what the CellLoop was a forward reference to. It
    * must be invoked inside the same transaction as the place where the CellLoop is used.
    * This requires you to create an explicit transaction with [[Transaction]].
    */
  def loop(a_out: Cell[A]): Unit = {
    val me = this
    Transaction(trans => {
      str match {
        case s: StreamLoop[A] => s.loop(a_out.updates(trans))
        case _                =>
      }
      me.lazyInitValue = Some(a_out.sampleLazy(trans))
      ()
    })
  }

  override def sampleNoTrans(): A = {
    str match {
      case s: StreamLoop[A] => if (!s.assigned) throw new RuntimeException("CellLoop sampled before it was looped")
      case _                =>
    }
    super.sampleNoTrans()
  }
}
