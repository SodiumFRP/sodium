package sodium

class StreamSink[A] extends StreamWithSend[A] {

  def send(a: A) {
    Transaction.run(trans => send(trans, a))
  }
}
