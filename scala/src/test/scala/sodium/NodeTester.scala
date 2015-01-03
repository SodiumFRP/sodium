package sodium

import junit.framework.Assert.assertTrue
import junit.framework.TestCase

class NodeTester extends TestCase {
  def testNode() {
    val a = new Node(0)
    val b = new Node(1)
    a.linkTo(b)
    assertTrue(a.compareTo(b) < 0)
  }
}
