package sodium

class StreamSink[A] extends StreamWithSend[A] {

  def send(a: A): Unit = {
    Transaction(trans => {
      if (Transaction.inCallback > 0)
        throw new RuntimeException("You are not allowed to use send() inside a Sodium callback")
      send(trans, a)
    })
  }
}
