package sodium

class StreamWithSend[A] extends Stream[A] {

  def send(trans: Transaction, a: A) {
    if (firings.isEmpty)
      trans.last(new Runnable() {
        def run() { firings.clear() }
      })
    firings += a

    try {
      listeners.clone.foreach(_.run(trans, a))
    } catch {
      case t: Throwable => t.printStackTrace()
    }
  }

}