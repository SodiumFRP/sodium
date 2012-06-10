package sodium;

import java.util.HashSet;
import java.util.Set;

public class Node implements Comparable<Node> {
    public final static Node NULL = new Node(Long.MAX_VALUE);

	Node(long rank) {
		this.rank = rank;
	}

	private long rank;
	private Set<Node> listeners = new HashSet<Node>();

	void linkTo(Node target) {
		if (target == NULL)
			return;

		target.ensureBiggerThan(rank, new HashSet<Node>());
		listeners.add(target);
	}

	void unlinkTo(Node target) {
		if (target == NULL)
			return;

		listeners.remove(target);
	}

	private void ensureBiggerThan(long limit, Set<Node> visited) {
		if (rank > limit || visited.contains(this))
			return;

		visited.add(this);
		rank = limit + 1;
		for (Node l : listeners)
			l.ensureBiggerThan(rank, visited);
	}

	@Override
	public int compareTo(Node o) {
		return Long.compare(rank, o.rank);
	}
}
