package sodium

class LazyCell[A](
  lazyInitValueFunction: () => A,
  event: Stream[A] = new Stream[A]())
  extends Cell[A](None, event) {

  protected override def sampleNoTrans(): Option[A] =
    value match {
      case None =>
        value = Some(lazyInitValue.get())
        value
      case _ => value
    }

}

