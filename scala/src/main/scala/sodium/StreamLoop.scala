package sodium

/**
  * A forward reference for a [[Stream]] equivalent to the Stream that is referenced.
  */
class StreamLoop[A] extends StreamWithSend[A] {

  var assigned = false

  if (Transaction.getCurrentTransaction().isEmpty) {
    throw new RuntimeException("StreamLoop/BehaviorLoop must be used within an explicit transaction")
  }

  /**
    * Resolve the loop to specify what the StreamLoop was a forward reference to. It
    * must be invoked inside the same transaction as the place where the StreamLoop is used.
    * This requires you to create an explicit transaction with [[Transaction]].
    */
  def loop(initStream: Stream[A]): Unit = {
    if (assigned) {
      throw new RuntimeException("StreamLoop looped more than once")
    }
    assigned = true
    Transaction(_ => {
      unsafeAddCleanup(initStream.listen_(this.node, (trans, a: A) => {
        StreamLoop.this.send(trans, a)
      }))
    })
    ()
  }
}
