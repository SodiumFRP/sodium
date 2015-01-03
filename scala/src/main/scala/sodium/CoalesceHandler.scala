package sodium

class CoalesceHandler[A](f: (A, A) => A, out: StreamSink[A])
  extends TransactionHandler[A] {
  private var accum: Option[A] = None

  override def run(trans1: Transaction, a: A) {
    if (accum.isDefined)
      accum = Some(f.apply(accum.get, a))
    else {
      val thiz = this
      trans1.prioritized(out.node, { 
          trans2 =>
          out.send(trans2, thiz.accum)
          thiz.accum = None
      })
      accum = Some(a)
    }
  }
}