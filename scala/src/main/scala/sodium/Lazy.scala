package sodium

/**
  * A representation for a value that may not be available until the current
  * transaction is closed.
  */
class Lazy[A](val f: () => A) {

  def this(a: A) = this(() => a)

  /**
    * Get the value if available, throwing an exception if not.
    * In the general case this should only be used in subsequent transactions to
    * when the Lazy was obtained.
    */
  final def get: A = f.apply()

  /**
    * Map the lazy value according to the specified function, so the returned Lazy reflects
    * the value of the function applied to the input Lazy's value.
    *
    * @param f2 Function to apply to the contained value. It must be <em>referentially transparent</em>.
    */
  final def map[B](f2: A => B): Lazy[B] =
    new Lazy[B](() => f2(f()))
}

object Lazy {

  /**
    * Lift a binary function into lazy values, so the returned Lazy reflects
    * the value of the function applied to the input Lazys' values.
    */
  def lift[A, B, C](f: (A, B) => C, a: Lazy[A], b: Lazy[B]): Lazy[C] =
    new Lazy[C](() => f(a.get, b.get))

  /**
    * Lift a ternary function into lazy values, so the returned Lazy reflects
    * the value of the function applied to the input Lazys' values.
    */
  def lift[A, B, C, D](f: (A, B, C) => D, a: Lazy[A], b: Lazy[B], c: Lazy[C]): Lazy[D] =
    new Lazy[D](() => f(a.get, b.get, c.get))

  /**
    * Lift a quaternary function into lazy values, so the returned Lazy reflects
    * the value of the function applied to the input Lazys' values.
    */
  def lift[A, B, C, D, E](f: (A, B, C, D) => E, a: Lazy[A], b: Lazy[B], c: Lazy[C], d: Lazy[D]): Lazy[E] =
    new Lazy[E](() => f(a.get, b.get, c.get, d.get))
}
