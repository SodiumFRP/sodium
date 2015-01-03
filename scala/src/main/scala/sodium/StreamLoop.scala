package sodium

class StreamLoop[A] extends Stream[A] {

  private var ea_out: Option[Stream[A]] = None

  if (Transaction.getCurrentTransaction() == null)
    throw new RuntimeException("StreamLoop/CellLoop must be used within an explicit transaction")

  protected def sampleNow(): IndexedSeq[A] = {
    if (ea_out.isEmpty)
      throw new RuntimeException("StreamLoop sampled before it was looped")
    ea_out.get.sampleNow()
  }

  // TODO: Copy & paste from StreamSink. Can we improve this?
  private def send(trans: Transaction, a: A) {
    if (firings.size == 0)
      trans.last(new Runnable() {
        def run() { firings.clear() }
      })
    firings += a

    try {
      listeners.foreach(_.run(trans, a))
    } catch {
      case t: Throwable => t.printStackTrace()
    }
  }

  def loop(ea_out: Stream[A]) {
    if (this.ea_out.isDefined)
      throw new RuntimeException("StreamLoop looped more than once")
    this.ea_out = Some(ea_out)
    val me = this
    addCleanup(ea_out.listen_(this.node, new TransactionHandler[A]() {
      override def run(trans: Transaction, a: A) {
        me.send(trans, a)
      }
    }))
  }
}

