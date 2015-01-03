package sodium

class LazyCell[A](
  lazyInitValueFunction: () => A,
  event: Stream[A] = new Stream[A]())
  extends Cell[A](None, event) {

  override def sampleNoTrans(): A =
    value match {
      case None =>
        value = Some(lazyInitValue.get())
        value.get
      case _ => value.get
    }

}

