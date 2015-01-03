package sodium

import scala.collection.mutable.HashSet

class Node(var rank: Long) extends Comparable[Node] {
  import Node._

  val listeners: HashSet[Node] = HashSet()

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

  private def ensureBiggerThan(limit: Long, visited: Set[Node]): Boolean = {
    if (rank > limit || visited.contains(this)) {
      false
    } else {
      val accVisited = Set(this) ++ visited
      rank = limit + 1
      listeners.forall(_.ensureBiggerThan(rank, visited))
    }
  }

  override def compareTo(o: Node): Int =
    if (rank < o.rank) -1
    else if (rank > o.rank) 1
    else 0
}

object Node {
  val NullNode = new Node(Long.MaxValue)
}
