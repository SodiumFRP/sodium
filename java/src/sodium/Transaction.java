package sodium;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;

public class Transaction {
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
		prioritizedQ.add(new Entry(rank, action));
	}

	public void last(Runnable action) {
	    lastQ.add(action);
	}

	public void close() {
		while (!prioritizedQ.isEmpty()) 
			prioritizedQ.remove().action.run(this);
		for (Runnable action : lastQ)
			action.run();
		lastQ.clear();
	}
}
