package sodium

class StreamSink[A] extends StreamWithSend[A] {

  def send(a: A): Unit = {
    Transaction.run(trans => send(trans, a))
  }
}
