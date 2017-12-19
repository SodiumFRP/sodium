package sodium

object Lazy {

  /**
    * Lift a binary function into lazy values.
    */
  def lift[A, B, C](f: (A, B) => C, a: Lazy[A], b: Lazy[B]) =
    new Lazy[C](() => f(a.get, b.get))

  /**
    * Lift a ternary function into lazy values.
    */
  def lift[A, B, C, D](f: (A, B, C) => D, a: Lazy[A], b: Lazy[B], c: Lazy[C]) =
    new Lazy[D](() => f(a.get, b.get, c.get))

  /**
    * Lift a quaternary function into lazy values.
    */
  def lift[A, B, C, D, E](f: (A, B, C, D) => E, a: Lazy[A], b: Lazy[B], c: Lazy[C], d: Lazy[D]) =
    new Lazy[E](() => f(a.get, b.get, c.get, d.get))
}

class Lazy[A](val f: () => A) {

  def this(a: A) = this(() => a)

  final def get: A = f.apply()

  /**
    * Map the lazy value according to the specified function.
    */
  final def map[B](f2: A => B) =
    new Lazy[B](() => f2(f()))
}
