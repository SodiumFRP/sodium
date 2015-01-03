package sodium

final class CellLoop[A] extends Cell[A](None, new StreamLoop[A]()) {

  def loop(a_out: Cell[A]) {
    event.asInstanceOf[StreamLoop[A]].loop(a_out.updates())
    value = a_out.sample()
  }

  protected override def sampleNoTrans(): Option[A] = {
    if (value.isEmpty)
      throw new RuntimeException("CellLoop sampled before it was looped")
    value
  }
}

