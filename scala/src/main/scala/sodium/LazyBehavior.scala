package sodium

class LazyCell[A](event: Stream[A], var lazyInitValue: () => A)
  extends Cell[A](event, null) {

  protected override def sampleNoTrans(): Option[A] = {
    if (value == null) {
      value = Some(lazyInitValue())
      lazyInitValue = null
    }
    value
  }
}

