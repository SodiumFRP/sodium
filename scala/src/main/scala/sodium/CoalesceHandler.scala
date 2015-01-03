package sodium

class CoalesceHandler[A](f: (A, A) => A, out: StreamSink[A])
  extends TransactionHandler[A] {
  private var accumValid = false
  private var accum: A = _

  override def run(trans1: Transaction, a: A) {
    if (accumValid)
      accum = f.apply(accum, a)
    else {
      val thiz = this
      trans1.prioritized(out.node, new Handler[Transaction]() {
        def run(trans2: Transaction) {
          out.send(trans2, thiz.accum)
          thiz.accumValid = false
          thiz.accum = null
        }
      })
      accum = a
      accumValid = true
    }
  }
}