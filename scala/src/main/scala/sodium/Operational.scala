package sodium

/**
  * Operational primitives that must be used with care because they
  * break non-detectability of cell steps/updates.
  */
object Operational {

  /**
    * A stream that gives the updates/steps for a [[Cell]].
    *
    * This is an OPERATIONAL primitive, which is not part of the main Sodium
    * API. It breaks the property of non-detectability of cell steps/updates.
    * The rule with this primitive is that you should only use it in functions
    * that do not allow the caller to detect the cell updates.
    */
  final def updates[A](c: Cell[A]) = Transaction(trans => c.updates(trans))

  /**
    * A stream that is guaranteed to fire once when you listen to it, giving
    * the current value of the cell, and thereafter behaves like [[updates update(Cell)]],
    * firing for each update/step of the cell's value.
    *
    * This is an OPERATIONAL primitive, which is not part of the main Sodium
    * API. It breaks the property of non-detectability of cell steps/updates.
    * The rule with this primitive is that you should only use it in functions
    * that do not allow the caller to detect the cell updates.
    */
  final def value[A](c: Cell[A]) = Transaction(trans => c.value(trans))
}
