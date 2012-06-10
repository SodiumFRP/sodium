package sodium;

import junit.framework.TestCase;

public class NodeTester extends TestCase {
	public void testNode() {
		Node a = new Node(0);
		Node b = new Node(1);
		a.linkTo(b);
		assertTrue(a.compareTo(b) < 0);
	}
}
