package sodium

import scala.collection.mutable

/**
  * Operational primitives that must be used with care.
  */
object Operational {

  /**
    * A stream that gives the updates/steps for a [[Behavior]].
    *
    * This is an OPERATIONAL primitive, which is not part of the main Sodium
    * API. It breaks the property of non-detectability of cell steps/updates.
    * The rule with this primitive is that you should only use it in functions
    * that do not allow the caller to detect the cell updates.
    */
  def updates[A](b: Behavior[A]): Stream[A] =
    Transaction(trans => b.updates().coalesce(trans, (_, right) => right))

  /**
    * A stream that is guaranteed to fire once in the transaction where value() is invoked, giving
    * the current value of the cell, and thereafter behaves like [[updates update(Behavior)]],
    * firing for each update/step of the cell's value.
    *
    * This is an OPERATIONAL primitive, which is not part of the main Sodium
    * API. It breaks the property of non-detectability of cell steps/updates.
    * The rule with this primitive is that you should only use it in functions
    * that do not allow the caller to detect the cell updates.
    */
  def value[A](b: Behavior[A]): Stream[A] = Transaction(trans => b.value(trans))

  /**
    * Push each event onto a new transaction guaranteed to come before the next externally
    * initiated transaction. Same as [[sodium.Operational.split* split(Stream)]] but it works on a single value.
    */
  final def defer[A](s: Stream[A]): Stream[A] =
    split(s.map(a => {
      val l = mutable.ListBuffer[A]()
      l += a
      l
    }))

  /**
    * Push each event in the list onto a newly created transaction guaranteed
    * to come before the next externally initiated transaction. Note that the semantics
    * are such that two different invocations of split() can put events into the same
    * new transaction, so the resulting stream's events could be simultaneous with
    * events output by split() or [[sodium.Operational.defer* defer(Stream)]] invoked elsewhere in the code.
    */
  def split[A, C <: Iterable[A]](s: Stream[C]): Stream[A] = {
    val out = new StreamWithSend[A]()
    val l1 = s.listen_(out.node, (trans: Transaction, as: C) => {
      var childIx = 0
      for (a <- as) {
        trans.post_(childIx, trans1 => out.send(trans1, a))
        childIx += 1
      }
    })
    out.unsafeAddCleanup(l1)
  }
}
