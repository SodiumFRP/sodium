package sodium

import scala.collection.mutable

class StreamWithSend[A] extends Stream[A] {

  def send(trans: Transaction, a: A): Unit = {
    if (firings.isEmpty) {
      trans.last(() => {
        firings.clear()
      })
    }
    firings += a

    var listeners = mutable.HashSet[Node.Target]()
    Transaction.listenersLock.synchronized {
      listeners = mutable.HashSet() ++ node.listeners
    }

    for (target <- listeners) {
      trans.prioritized(
        target.node,
        trans2 => {
          Transaction.inCallback += 1
          try {
            // Don't allow transactions to interfere with Sodium internals.
            // Dereference the weak reference
            val uta = target.action.get
            if (uta.isDefined) {
              // If it hasn't been gc'ed..., call it
              uta.get match {
                case t: TransactionHandler[_] => t.asInstanceOf[TransactionHandler[A]].run(trans2, a)
                case _                        =>
              }
            }
          } catch {
            case t: Throwable =>
              t.printStackTrace()
          } finally {
            Transaction.inCallback -= 1
          }
        }
      )
    }
  }
}
