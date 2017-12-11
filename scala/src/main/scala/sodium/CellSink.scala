package sodium

final class CellSink[A](initValue: A) extends Cell[A](Some(initValue), new StreamSink[A]()) {

  def send(a: A): Unit = {
    event match {
      case s: StreamSink[A] => s.send(a)
      case _                =>
    }
  }
}
