package sodium

class StreamWithSend[A] extends Stream[A] {

  def send(trans: Transaction, a: A): Unit = {
    if (firings.isEmpty)
      trans.last(new Runnable() {
        def run(): Unit = { firings.clear() }
      })
    firings += a

    try {
      listeners.clone.foreach(_.run(trans, a))
    } catch {
      case t: Throwable => t.printStackTrace()
    }
  }

}
