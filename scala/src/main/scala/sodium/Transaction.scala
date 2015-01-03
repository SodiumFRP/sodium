package sodium

import scala.collection.mutable.PriorityQueue
import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

final class Transaction {
  import Transaction._

  // True if we need to re-generate the priority queue.
  private [sodium] var toRegen = false

  private val prioritizedQ = new PriorityQueue[Entry]()
  private val entries = new HashSet[Entry]()
  private val lastQ: ListBuffer[Runnable] = ListBuffer()
  private val postQ: ListBuffer[Runnable] = ListBuffer()

  def prioritized(rank: Node, action: Transaction => Unit) {
    val e = new Entry(rank, action)
    prioritizedQ += e
    entries += e
  }

  /**
   * Add an action to run after all prioritized() actions.
   */
  def last(action: Runnable) {
    lastQ += action
  }

  /**
   * Add an action to run after all last() actions.
   */
  def post(action: Runnable) {
    postQ += action
  }

  def close() {

    /**
     * If the priority queue has entries in it when we modify any of the nodes'
     * ranks, then we need to re-generate it to make sure it's up-to-date.
     */
    def checkRegen() {
      if (toRegen) {
        toRegen = false
        prioritizedQ.clear()
        prioritizedQ ++= entries
      }
    }

    var finished = false
    while (!finished) {
      checkRegen()
      if (prioritizedQ.size == 0) {
        finished = true
      } else {
        val e = prioritizedQ.dequeue()
        entries.remove(e)
        e.action(this)
      }
    }
    lastQ.foreach(_.run())
    lastQ.clear()
    postQ.foreach(_.run())
    postQ.clear()
  }
}

object Transaction {

  // Coarse-grained lock that's held during the whole transaction.
  val transactionLock = new Object()

  // Fine-grained lock that protects listeners and nodes.
  val listenersLock = new Object()

  var currentTransaction: Option[Transaction] = None

  private def doTransaction[A, B](f: Transaction => B): B = {
    transactionLock.synchronized {
      // If we are already inside a transaction (which must be on the same
      // thread otherwise we wouldn't have acquired transactionLock), then
      // keep using that same transaction.
      val transWas = currentTransaction
      try {
        if (currentTransaction == None)
          currentTransaction = Some(new Transaction())
        f(currentTransaction.get)
      } finally {
        if (transWas == None)
          currentTransaction.foreach(_.close())
        currentTransaction = transWas
      }
    }
  }

  /**
   * Run the specified code inside a single transaction, with the contained
   * code returning a value of the parameter type A.
   *
   * In most cases this is not needed, because all APIs will create their own
   * transaction automatically. It is useful where you want to run multiple
   * reactive operations atomically.
   */
  def run[A](code: () => A): A =
    doTransaction(_ => code.apply())

  /**
   * Run the specified code inside a single transaction, with the contained
   * code returning a value of the parameter type A.
   *
   * In most cases this is not needed, because all APIs will create their own
   * transaction automatically. It is useful where you want to run multiple
   * reactive operations atomically.
   */
  def run(code: Transaction => Unit) {
    doTransaction(transaction => code(transaction))
  }

  /**
   * Run the specified code inside a single transaction, with the contained
   * code returning a value of the parameter type A.
   *
   * In most cases this is not needed, because all APIs will create their own
   * transaction automatically. It is useful where you want to run multiple
   * reactive operations atomically.
   */
  def apply[A](code: Transaction => A): A = {
    doTransaction(transaction => code.apply(transaction))
  }

  /**
   * Run the specified code inside a single transaction.
   *
   * In most cases this is not needed, because all APIs will create their own
   * transaction automatically. It is useful where you want to run multiple
   * reactive operations atomically.
   */
  def rundef(code: Runnable) {
    doTransaction(_ => code.run())
  }

  /**
   * Return the current transaction.
   */
  def getCurrentTransaction(): Option[Transaction] =
    transactionLock.synchronized {
      currentTransaction
    }

  private object Entry {
    private val nextSeq = new AtomicLong(0)
  }

  private class Entry(val rank: Node, val action: Transaction => Unit)
    extends Comparable[Entry] {
    import Entry._
    private val seq = nextSeq.getAndIncrement()

    override def compareTo(o: Entry): Int = {
      val answer = rank.compareTo(o.rank)
      // Same rank: preserve chronological sequence.
      if (answer == 0 && seq < o.seq) -1
      else if (answer == 0 && seq > o.seq) 1
      else answer
    }
  }
}
