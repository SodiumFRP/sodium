package sodium

class CoalesceHandler[A](private val f: (A, A) => A, private val out: StreamWithSend[A]) extends TransactionHandler[A] {
  private var acc: Option[A] = None

  override def run(trans1: Transaction, a: A): Unit = {
    acc match {
      case Some(b) =>
        acc = Some(f(b, a))
      case None =>
        trans1.prioritized(out.node, { trans2 =>
          out.send(trans2, acc.get)
          acc = None
        })
        acc = Some(a)
    }
    ()
  }
}
