package sodium

class LazyBehavior[A](event: Stream[A], _lazyInitValue: Option[Lazy[A]]) extends Behavior[A](event, None) {

  this.lazyInitValue = _lazyInitValue

  override def sampleNoTrans(): A = {
    if (currentValue.isEmpty && lazyInitValue.isDefined) {
      currentValue = Some(lazyInitValue.get.f.apply())
      lazyInitValue = None
    }
    currentValue.get
  }
}
