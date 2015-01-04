package sodium

import scala.collection.mutable.HashSet

class Node(private var rank: Long) extends Comparable[Node] {
  import Node._

  private val listeners = HashSet[Node]()

  /**
   * @return true if any changes were made.
   */
  def linkTo(target: Node): Boolean =
    if (target == NullNode) {
      false
    } else {
      val changed = target.ensureBiggerThan(rank, Set())
      listeners.add(target)
      changed
    }

  def unlinkTo(target: Node) {
    if (target != NullNode)
      listeners.remove(target)
  }

  private def ensureBiggerThan(limit: Long, visited: Set[Node]): Boolean =
    if (rank > limit || visited.contains(this)) {
      false
    } else {
      val accVisited = Set(this) ++ visited
      rank = limit + 1
      listeners.foreach(_.ensureBiggerThan(rank, accVisited))
      true
    }

  override def compareTo(o: Node): Int = rank.compareTo(o.rank)
}

object Node {
  val NullNode = new Node(Long.MaxValue)
}
