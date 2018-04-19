package sodium

class LazyBehavior[A](event: Stream[A], _lazyInitValue: Option[Lazy[A]]) extends Behavior[A](event, None) {

  this.lazyInitValue = _lazyInitValue

  def this(trans: Transaction, event: Stream[A], _lazyInitValue: Option[Lazy[A]]) = {
    this(event, _lazyInitValue)
    trans.sample(() => ensureValueIsCreated())
  }

  private def ensureValueIsCreated(): Unit = {
    if (currentValue.isEmpty && lazyInitValue.isDefined) {
      currentValue = Some(lazyInitValue.get.f.apply())
      lazyInitValue = None
    }
  }

  override def sampleNoTrans(): A = {
    ensureValueIsCreated()
    currentValue.get
  }
}
