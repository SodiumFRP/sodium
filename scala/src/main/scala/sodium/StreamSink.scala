package sodium

class StreamSink[A] extends Stream[A] {

  def send(a: A) {
    Transaction.run(trans => send(trans, a))
  }

  def send(trans: Transaction, a: A) {
    if (firings.isEmpty)
      trans.last(new Runnable() {
        def run() { firings.clear() }
      })
    firings += a

    // todo shouldn't need to clone here ...

    try {
      listeners.foreach(_.run(trans, a))
    } catch {
      case t: Throwable => t.printStackTrace()
    }
  }
}
