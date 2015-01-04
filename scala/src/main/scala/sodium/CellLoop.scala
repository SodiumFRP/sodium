package sodium

final class CellLoop[A] extends Cell[A](None, new StreamLoop[A]()) {

  def loop(a_out: Cell[A]) {
    event.asInstanceOf[StreamLoop[A]].loop(a_out.updates())
    currentValue = Some(a_out.sample())
  }

  override def sampleNoTrans(): A = {
    if (currentValue.isEmpty)
      throw new RuntimeException("CellLoop sampled before it was looped")
    currentValue.get
  }
}

