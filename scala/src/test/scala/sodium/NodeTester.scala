package sodium

import org.junit.Assert.assertTrue
import org.junit.Test

class NodeTester {
  @Test
  def testNode() {
    val a = new Node(0)
    val b = new Node(1)
    a.linkTo(b)
    assertTrue(a.compareTo(b) < 0)
  }
}
