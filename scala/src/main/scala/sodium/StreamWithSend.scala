package sodium

import scala.collection.mutable.HashSet

class StreamWithSend[A] extends Stream[A] {

  def send(trans: Transaction, a: A): Unit = {
    if (firings.isEmpty)
      trans.last(new Runnable() {
        def run(): Unit = {
          firings.clear()
        }
      })
    firings += a

    var listeners = HashSet[Node.Target]()
    Transaction.listenersLock.synchronized {
      listeners = HashSet() ++ node.listeners
    }

    for (target <- node.listeners) {
      trans.prioritized(
        target.node,
        trans2 =>
          try // Don't allow transactions to interfere with Sodium
          // internals.
          target.action.asInstanceOf[TransactionHandler[A]].run(trans, a)
          catch {
            case t: Throwable =>
              t.printStackTrace()
        }
      )
    }
  }
}
//}

//}
