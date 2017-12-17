package sodium

class StreamLoop[A] extends StreamWithSend[A] {

  var assigned = false

  if (Transaction.getCurrentTransaction() == None)
    throw new RuntimeException("StreamLoop/CellLoop must be used within an explicit transaction")

  def loop(initStream: Stream[A]): Unit = {
    if (assigned)
      throw new RuntimeException("StreamLoop looped more than once")
    assigned = true
    unsafeAddCleanup(initStream.listen_(this.node, new TransactionHandler[A]() {
      override def run(trans: Transaction, a: A): Unit = {
        StreamLoop.this.send(trans, a)
      }
    }))
    ()
  }
}
