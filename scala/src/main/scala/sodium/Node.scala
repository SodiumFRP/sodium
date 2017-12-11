package sodium

import scala.collection.mutable.ListBuffer

class Node(private var rank: Long) extends Comparable[Node] {
  import Node._

  val listeners = ListBuffer[Target]()

  /**
    * @return true if any changes were made.
    */
  def linkTo(action: TransactionHandler[Unit], target: Node): Boolean = {
    val changed = target.ensureBiggerThan(rank, Set())
    listeners += Target(action, target)
    changed
  }

  def unlinkTo(target: Node): Unit = {
    listeners.collectFirst { case t if t.node == target => listeners -= t }
    ()
  }

  private def ensureBiggerThan(limit: Long, visited: Set[Node]): Boolean =
    if (rank > limit || visited.contains(this)) {
      false
    } else {
      val accVisited = Set(this) ++ visited
      rank = limit + 1
      listeners.foreach(_.node.ensureBiggerThan(rank, accVisited))
      true
    }

  override def compareTo(o: Node): Int = rank.compareTo(o.rank)
}

object Node {
  val NullNode = new Node(Long.MaxValue)

  case class Target(var action: TransactionHandler[Unit], var node: Node)

}
