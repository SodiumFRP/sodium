package sodium;

import java.util.ArrayList;
import java.util.List;
import java.util.HashSet;
import java.util.Set;

public class Node implements Comparable<Node> {
    public final static Node NULL = new Node(Long.MAX_VALUE);

	Node(long rank) {
		this.rank = rank;
	}

	public static class Target {
	    Target(TransactionHandler<Unit> action, Node node) {
	        this.action = action;
	        this.node = node;
	    }
	    final TransactionHandler<Unit> action;
	    final Node node;
    }

	private long rank;
    List<Target> listeners = new ArrayList<>();

	/**
	 * @return true if any changes were made. 
	 */
	boolean linkTo(TransactionHandler<Unit> action, Node target) {
		boolean changed = target.ensureBiggerThan(rank, new HashSet<Node>());
		listeners.add(new Target(action, target));
		return changed;
	}

	void unlinkTo(Node target) {
	    for (int i = 0; i < listeners.size(); i++)
            if (listeners.get(i).node == target) {
                listeners.remove(i);
                break;
            }
	}

	private boolean ensureBiggerThan(long limit, Set<Node> visited) {
		if (rank > limit || visited.contains(this))
			return false;

		visited.add(this);
		rank = limit + 1;
		for (Target l : listeners)
			l.node.ensureBiggerThan(rank, visited);
		return true;
	}

	@Override
	public int compareTo(Node o) {
		if (rank < o.rank) return -1;
		if (rank > o.rank) return 1;
		return 0;
	}
}
