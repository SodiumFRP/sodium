package sodium

class StreamSink[A] extends StreamWithSend[A] {

  def send(a: A): Unit = {
    Transaction(trans => send(trans, a))
  }
}
