package sodium

final class CellSink[A](initValue: Option[A])
  extends Cell[A](new StreamSink[A](), initValue) {

  def send(a: A) {
    event.asInstanceOf[StreamSink[A]].send(a)
  }
}
