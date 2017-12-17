package sodium

final class CellSink[A](initValue: A) extends Cell[A](new StreamSink[A](), Some(initValue)) {

  def send(a: A): Unit = {
    str match {
      case s: StreamSink[A] => s.send(a)
      case _                =>
    }
  }
}
