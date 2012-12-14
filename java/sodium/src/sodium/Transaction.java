package sodium;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.HashSet;

public final class Transaction {
    private static final Object lock = new Object();

    // True if we need to re-generate the priority queue.
    boolean toRegen = false;

	private static class Entry implements Comparable<Entry> {
		private Node rank;
		private Handler<Transaction> action;
		private static long nextSeq;
		private long seq;

		public Entry(Node rank, Handler<Transaction> action) {
			this.rank = rank;
			this.action = action;
			this.seq = nextSeq++;
		}

		@Override
		public int compareTo(Entry o) {
			int answer = rank.compareTo(o.rank);
			if (answer == 0) {  // Same rank: preserve chronological sequence.
				if (seq < o.seq) answer = -1; else
				if (seq > o.seq) answer = 1;
			}
			return answer;
		}

	}

	private PriorityQueue<Entry> prioritizedQ = new PriorityQueue<Entry>();
	private Set<Entry> entries = new HashSet<Entry>();
	private List<Runnable> lastQ = new ArrayList<Runnable>(); 
	private List<Runnable> postQ;

	Transaction() {
	}

	public static void run(Handler<Transaction> code) {
	    synchronized (lock) {
            Transaction trans = new Transaction();
            try {
                code.run(trans);
            } finally {
                trans.close();
            }
        }
	}

	public static <A> A apply(Lambda1<Transaction, A> code) {
	    synchronized (lock) {
            Transaction trans = new Transaction();
            try {
                return code.apply(trans);
            } finally {
                trans.close();
            }
        }
	}

	public void prioritized(Node rank, Handler<Transaction> action) {
	    Entry e = new Entry(rank, action);
		prioritizedQ.add(e);
		entries.add(e);
	}

	/**
     * Add an action to run after all prioritized() actions.
     */
	public void last(Runnable action) {
	    lastQ.add(action);
	}

	/**
     * Add an action to run after all last() actions.
     */
	public void post(Runnable action) {
	    if (postQ == null)
	        postQ = new ArrayList<Runnable>();
	    postQ.add(action);
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
		if (postQ != null) {
            for (Runnable action : postQ)
                action.run();
            postQ.clear();
		}
	}
}
