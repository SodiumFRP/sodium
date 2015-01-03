package sodium

class LazyCell[A](event: Stream[A], var lazyInitValue: Option[() => A])
  extends Cell[A](event, null) {

  protected override def sampleNoTrans(): Option[A] = {
    if (value == None && lazyInitValue.isDefined) {
      value = Some(lazyInitValue.get())
    }
    lazyInitValue = None
    value
  }
}

