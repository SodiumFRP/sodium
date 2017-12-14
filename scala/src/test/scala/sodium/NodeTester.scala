package sodium

import org.junit.Assert.assertTrue
import org.junit.Test
import sodium.Node.Target

class NodeTester {
  @Test
  def testNode(): Unit = {
    val a = new Node(0)
    val b = new Node(1)
    val node_target_ = new Array[Target](1)
    a.linkTo(null, b, node_target_)
    assertTrue(a.compareTo(b) < 0)
  }
}
