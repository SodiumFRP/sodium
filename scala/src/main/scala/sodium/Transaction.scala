package sodium

import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue

/**
  * Functions for controlling transactions.
  */
final class Transaction {
  import Transaction._

  // True if we need to re-generate the priority queue.
  private[sodium] var toRegen = false

  private final val prioritizedQ = new PriorityQueue[Entry]()(EntryOrdering)
  private final val entries = new HashSet[Entry]()
  private final val lastQ = ListBuffer[Runnable]()
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
  final val transactionLock = new Object()

  // Fine-grained lock that protects listeners and nodes.
  final val listenersLock = new Object()

  var currentTransaction: Option[Transaction] = None
  var inCallback: Int = 0
  private val onStartHooks = new ListBuffer[Runnable]
  private var runningOnStartHooks = false

  /**
    * Add a runnable that will be executed whenever a transaction is started.
    * That runnable may start transactions itself, which will not cause the
    * hooks to be run recursively.
    *
    * The main use case of this is the implementation of a time/alarm system.
    */
  def onStart(r: Runnable): Unit = {
    transactionLock.synchronized {
      onStartHooks += r
      ()
    }
  }

  /**
    * Run the specified code inside a single transaction, with the contained
    * code returning a value of the parameter type A.
    *
    * In most cases this is not needed, because the primitives always create their own
    * transaction automatically, but it is needed in some circumstances.
    */
  def apply[A](code: Transaction => A): A = {
    transactionLock.synchronized {
      // If we are already inside a transaction (which must be on the same
      // thread otherwise we wouldn't have acquired transactionLock), then
      // keep using that same transaction.
      val transWas = currentTransaction
      try {
        startIfNecessary()
        code(currentTransaction.get)
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

  private def startIfNecessary(): Unit = {
    if (currentTransaction == None) {
      if (!runningOnStartHooks) {
        runningOnStartHooks = true
        try {
          for (r <- onStartHooks) {
            r.run()
          }
        } finally {
          runningOnStartHooks = false
        }
      }
      currentTransaction = Some(new Transaction())
    }
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

  private case class Entry(final val rank: Node, final val action: Transaction => Unit) {
    val seq = Entry.nextSeq.getAndIncrement()
  }

  private object EntryOrdering extends Ordering[Entry] {
    def compare(x: Entry, y: Entry): Int = {
      val answer = y.rank.compareTo(x.rank)
      if (answer == 0) y.seq.compareTo(x.seq) else answer
    }
  }
}
