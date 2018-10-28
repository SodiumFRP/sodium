package sodium

import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference

class Node(private var rank: Long = 0) extends Comparable[Node] {
  import Node._

  val listeners: ListBuffer[Target] = ListBuffer[Target]()

  /**
    * @return true if any changes were made.
    */
  def linkTo(action: TransactionHandler[Unit], target: Node, outTarget: Array[Target]): Boolean = {
    val changed = target.ensureBiggerThan(rank, Set())
    val t = Target(action, target)
    listeners += t
    outTarget(0) = t
    changed
  }

  def unlinkTo(target: Target): Unit = {
    listeners -= target
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

  case class Target(final var action: WeakReference[TransactionHandler[Unit]], final var node: Node)

  object Target {
    def apply(action: TransactionHandler[Unit], node: Node): Target = new Target(new WeakReference(action), node)
  }

}
