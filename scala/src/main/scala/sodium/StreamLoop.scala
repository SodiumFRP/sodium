package sodium

class StreamLoop[A] extends Stream[A] {
  
  private var ea_out: Stream[A] = _

  if (Transaction.getCurrentTransaction() == null)
    throw new RuntimeException("StreamLoop/CellLoop must be used within an explicit transaction")

  protected def sampleNow(): IndexedSeq[A] = {
    if (ea_out == null)
      throw new RuntimeException("StreamLoop sampled before it was looped")
    ea_out.sampleNow()
  }

  // TO DO: Copy & paste from StreamSink. Can we improve this?
  private def send(trans: Transaction, a: A) {
    if (firings.size == 0)
      trans.last(new Runnable() {
        def run() { firings = List() }
      })
    firings = firings ++ List(a)

    try {
      listeners.foreach(_.run(trans, a))
    } catch {
      case t: Throwable => t.printStackTrace()
    }
  }

  def loop(ea_out: Stream[A]) {
    if (this.ea_out != null)
      throw new RuntimeException("StreamLoop looped more than once")
    this.ea_out = ea_out
    val me = this
    addCleanup(ea_out.listen_(this.node, (trans, a) => me.send(trans, a))
  }
}

