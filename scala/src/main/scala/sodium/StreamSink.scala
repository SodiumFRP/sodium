package sodium

class StreamSink[A] extends Stream[A] {

  def send(a: A) {
    Transaction.run(new Handler[Transaction]() {
      def run(trans: Transaction) { send(trans, a) }
    })
  }

  def send(trans: Transaction, a: A) {
    if (firings.isEmpty())
      trans.last(new Runnable() {
        def run() { firings.clear() }
      })
    firings.add(a)

    // todo shouldn't need to clone here ...

    try {
      this.listeners.clone().asInstanceOf[List[TransactionHandler[A]]].foreach(_.run(trans, a))
    } catch {
      case t: Throwable => t.printStackTrace()
    }
  }
}
