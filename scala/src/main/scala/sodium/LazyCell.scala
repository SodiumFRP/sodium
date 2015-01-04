package sodium

class LazyCell[A](lazyInitValueFunction: () => A, event: Stream[A]) extends Cell[A](None, event) {

  override def sampleNoTrans(): A =
    currentValue match {
      case None =>
        currentValue = Some(lazyInitValueFunction())
        currentValue.get
      case _ => currentValue.get
    }

}

