package sodium

import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue

final class Transaction {
  import Transaction._

  // True if we need to re-generate the priority queue.
  private[sodium] var toRegen = false

  private val prioritizedQ = new PriorityQueue[Entry]()(EntryOrdering)
  private val entries = new HashSet[Entry]()
  private val lastQ = ListBuffer[Runnable]()
  private val postQ = ListBuffer[Runnable]()

  def prioritized(rank: Node, action: Transaction => Unit): Unit = {
    val e = new Entry(rank, action)
    prioritizedQ.enqueue(e)
    entries += e
  }

  /**
    * Add an action to run after all prioritized() actions.
    */
  def last(action: Runnable): Unit = {
    lastQ += action
  }

  /**
    * Add an action to run after all last() actions.
    */
  def post(action: Runnable): Unit = {
    postQ += action
  }

  def close(): Unit = {

    /*
     * If the priority queue has entries in it when we modify any of the nodes'
     * ranks, then we need to re-generate it to make sure it's up-to-date.
     */
    def checkRegen(): Unit = {
      if (toRegen) {
        toRegen = false
        prioritizedQ.clear()
        prioritizedQ.enqueue(entries.toSeq: _*)
      }
    }

    while (!prioritizedQ.isEmpty) {
      checkRegen()
      val e = prioritizedQ.dequeue()
      entries.remove(e)
      e.action(this)
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

  /**
    * Run the specified code inside a single transaction, with the contained
    * code returning a value of the parameter type A.
    *
    * In most cases this is not needed, because all APIs will create their own
    * transaction automatically. It is useful where you want to run multiple
    * reactive operations atomically.
    */
  def apply[B](f: Transaction => B): B = {
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

  //not needed in Scala
  //def run(f: Transaction => Unit): Unit = {
  //  apply(t => f(t))
  //}

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

  private case class Entry(val rank: Node, val action: Transaction => Unit) {
    val seq = Entry.nextSeq.getAndIncrement()
  }

  private object EntryOrdering extends Ordering[Entry] {
    def compare(x: Entry, y: Entry): Int = {
      val answer = y.rank.compareTo(x.rank)
      if (answer == 0) y.seq.compareTo(x.seq) else answer
    }
  }
}
