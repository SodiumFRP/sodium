package sodium

class StreamLoop[A] extends StreamWithSend[A] {

  private var ea_out: Option[Stream[A]] = None

  if (Transaction.getCurrentTransaction() == None)
    throw new RuntimeException("StreamLoop/CellLoop must be used within an explicit transaction")

  def loop(initStream: Stream[A]): Unit = {
    if (ea_out.isDefined)
      throw new RuntimeException("StreamLoop looped more than once")
    ea_out = Some(initStream)
    unsafeAddCleanup(initStream.listen_(this.node, new TransactionHandler[A]() {
      override def run(trans: Transaction, a: A): Unit = {
        StreamLoop.this.send(trans, a)
      }
    }))
    ()
  }
}
