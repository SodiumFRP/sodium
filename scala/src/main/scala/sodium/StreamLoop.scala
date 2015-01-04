package sodium

class StreamLoop[A] extends StreamWithSend[A] {

  private var ea_out: Option[Stream[A]] = None

  if (Transaction.getCurrentTransaction() == None)
    throw new RuntimeException("StreamLoop/CellLoop must be used within an explicit transaction")

  override def sampleNow() =
    if (ea_out.isEmpty)
      throw new RuntimeException("StreamLoop sampled before it was looped")
    else
      ea_out.get.sampleNow()

  def loop(initStream: Stream[A]) {
    if (ea_out.isDefined)
      throw new RuntimeException("StreamLoop looped more than once")
    ea_out = Some(initStream)
    addCleanup(initStream.listen_(this.node, new TransactionHandler[A]() {
      override def run(trans: Transaction, a: A) {
        StreamLoop.this.send(trans, a)
      }
    }))
  }
}

