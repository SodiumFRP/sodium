package sodium

class LazyCell[A](event: Stream[A], _lazyInitValue: Option[Lazy[A]]) extends Cell[A](event, None) {

  this.lazyInitValue = _lazyInitValue

  override /*protected*/ def sampleNoTrans(): A = {
    if (currentValue.isEmpty && lazyInitValue.isDefined) {
      currentValue = Some(lazyInitValue.get.f.apply())
      lazyInitValue = None
    }
    currentValue.get
  }
}
