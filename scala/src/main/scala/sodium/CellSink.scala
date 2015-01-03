package sodium

final class CellSink[A](initValue: Option[A] = None)
  extends Cell[A](new StreamSink[A](), initValue) {
  
  def this(initValue : A) {
    this(Some(initValue))
  }

  def send(a: A) {
    event.asInstanceOf[StreamSink[A]].send(a)
  }
}
