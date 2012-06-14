package sodium;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.HashSet;

public class Transaction {
    // True if we need to re-generate the priority queue.
    boolean toRegen = false;

	private static class Entry implements Comparable<Entry> {
		private Node rank;
		private Handler<Transaction> action;

		public Entry(Node rank, Handler<Transaction> action) {
			this.rank = rank;
			this.action = action;
		}

		@Override
		public int compareTo(Entry o) {
			return rank.compareTo(o.rank);
		}

	}

	private PriorityQueue<Entry> prioritizedQ = new PriorityQueue<Entry>();
	private Set<Entry> entries = new HashSet<Entry>();
	private List<Runnable> lastQ = new ArrayList<Runnable>(); 

	private Transaction() {
	}

	public static void run(Handler<Transaction> code) {
		Transaction trans = new Transaction();
		try {
			code.run(trans);
		} finally {
			trans.close();
		}
	}

	public static <A> A evaluate(Lambda1<Transaction, A> code) {
		Transaction trans = new Transaction();
		try {
			return code.evaluate(trans);
		} finally {
			trans.close();
		}
	}

	public void prioritized(Node rank, Handler<Transaction> action) {
	    Entry e = new Entry(rank, action);
		prioritizedQ.add(e);
		entries.add(e);
	}

	public void last(Runnable action) {
	    lastQ.add(action);
	}

	/**
	 * If the priority queue has entries in it when we modify any of the nodes'
	 * ranks, then we need to re-generate it to make sure it's up-to-date.
	 */
	private void checkRegen()
	{
	    if (toRegen) {
	        toRegen = false;
	        prioritizedQ.clear();
	        for (Entry e : entries)
	            prioritizedQ.add(e);
	    }
	}

	public void close() {
	    while (true) {
	        checkRegen();
		    if (prioritizedQ.isEmpty()) break;
		    Entry e = prioritizedQ.remove();
		    entries.remove(e);
			e.action.run(this);
		}
		for (Runnable action : lastQ)
			action.run();
		lastQ.clear();
	}
}
