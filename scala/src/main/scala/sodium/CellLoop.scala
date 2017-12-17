package sodium

final class CellLoop[A] extends LazyCell[A](new StreamLoop[A](), None) {

  def loop(a_out: Cell[A]): Unit = {
    str match {
      case s: StreamLoop[A] => s.loop(a_out.updates())
      case _                =>
    }
    this.lazyInitValue = Some(a_out.sampleLazy())
  }

  override def sampleNoTrans(): A = {
    str match {
      case s: StreamLoop[A] => if (!s.assigned) throw new RuntimeException("CellLoop sampled before it was looped")
      case _                =>
    }
    super.sampleNoTrans()
  }
}
