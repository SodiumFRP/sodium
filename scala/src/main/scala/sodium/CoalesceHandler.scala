package sodium

class CoalesceHandler[A](f: (A, A) => A, out: StreamSink[A])
  extends TransactionHandler[A] {
  private var acc: Option[A] = None

  override def run(trans1: Transaction, a: A) {
    acc match {
      case Some(b) => acc = Some(f.apply(b, a))
      case _ =>
        trans1.prioritized(out.node,
          {
            trans2 =>
              out.send(trans2, acc.get)
              acc = None
          })
        acc = Some(a)
    }
  }
}