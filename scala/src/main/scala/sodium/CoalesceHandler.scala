package sodium

class CoalesceHandler[A](f: (A, A) => A, out: StreamSink[A])
  extends TransactionHandler[A] {
  private var accum: Option[A] = None

  override def run(trans1: Transaction, a: A) {
    if (accum.isDefined)
      accum = Some(f.apply(accum.get, a))
    else {
      trans1.prioritized(out.node,
        {
          trans2 =>
            out.send(trans2, accum.get)
            accum = None
        })
      accum = Some(a)
    }
  }
}