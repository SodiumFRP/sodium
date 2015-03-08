package sodium;

import junit.framework.TestCase;

public class NodeTester extends TestCase {
	public void testNode() {
		Node a = new Node(0);
		Node b = new Node(1);
		Node.Target[] node_target_ = new Node.Target[1];
		a.linkTo(null, b, node_target_);
		assertTrue(a.compareTo(b) < 0);
	}
}
