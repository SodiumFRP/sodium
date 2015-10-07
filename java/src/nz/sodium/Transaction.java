package nz.sodium;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;


/**
 * Functions for controlling transactions.
 */
public final class Transaction {
    // Coarse-grained lock that's held during the whole transaction.
    static final Object transactionLock = new Object();
    // Fine-grained lock that protects listeners and nodes.
    static final Object listenersLock = new Object();

    // True if we need to re-generate the priority queue.
    boolean toRegen = false;

	private static class Entry implements Comparable<Entry> {
		private final Node rank;
		private final Handler<Transaction> action;
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

	private final PriorityQueue<Entry> prioritizedQ = new PriorityQueue<Entry>();
	private final Set<Entry> entries = new HashSet<Entry>();
	private final List<Runnable> lastQ = new ArrayList<Runnable>();
	private Map<Integer, Handler<Transaction>> postQ;

	Transaction() {
	}

	private static Transaction currentTransaction;
    static int inCallback;
    private static List<Runnable> onStartHooks = new ArrayList<Runnable>();
    private static boolean runningOnStartHooks = false;

	/**
	 * Return the current transaction, or null if there isn't one.
	 */
	static Transaction getCurrentTransaction() {
        synchronized (transactionLock) {
            return currentTransaction;
        }
	}

	/**
	 * Run the specified code inside a single transaction.
	 *
	 * In most cases this is not needed, because the primitives always create their own
	 * transaction automatically, but it is needed in some circumstances.
	 */
	public static void runVoid(Runnable code) {
        synchronized (transactionLock) {
            // If we are already inside a transaction (which must be on the same
            // thread otherwise we wouldn't have acquired transactionLock), then
            // keep using that same transaction.
            Transaction transWas = currentTransaction;
            try {
                startIfNecessary();
                code.run();
            } finally {
                if (transWas == null)
                    currentTransaction.close();
                currentTransaction = transWas;
            }
        }
	}

	/**
	 * Run the specified code inside a single transaction, with the contained
	 * code returning a value of the parameter type A.
	 *
	 * In most cases this is not needed, because the primitives always create their own
	 * transaction automatically, but it is needed in some circumstances.
	 */
	public static <A> A run(Lambda0<A> code) {
        synchronized (transactionLock) {
            // If we are already inside a transaction (which must be on the same
            // thread otherwise we wouldn't have acquired transactionLock), then
            // keep using that same transaction.
            Transaction transWas = currentTransaction;
            try {
                startIfNecessary();
                return code.apply();
            } finally {
                if (transWas == null)
                    currentTransaction.close();
                currentTransaction = transWas;
            }
        }
	}

	static void run(Handler<Transaction> code) {
        synchronized (transactionLock) {
            // If we are already inside a transaction (which must be on the same
            // thread otherwise we wouldn't have acquired transactionLock), then
            // keep using that same transaction.
            Transaction transWas = currentTransaction;
            try {
                startIfNecessary();
                code.run(currentTransaction);
            } finally {
                if (transWas == null)
                    currentTransaction.close();
                currentTransaction = transWas;
            }
        }
	}

	/**
	 * Add a runnable that will be executed whenever a transaction is started.
	 * That runnable may start transactions itself, which will not cause the
	 * hooks to be run recursively.
	 *
	 * The main use case of this is the implementation of a time/alarm system.
	 */
	public static void onStart(Runnable r) {
        synchronized (transactionLock) {
            onStartHooks.add(r);
        }
	}

	static <A> A apply(Lambda1<Transaction, A> code) {
        synchronized (transactionLock) {
            // If we are already inside a transaction (which must be on the same
            // thread otherwise we wouldn't have acquired transactionLock), then
            // keep using that same transaction.
            Transaction transWas = currentTransaction;
            try {
                startIfNecessary();
                return code.apply(currentTransaction);
            } finally {
                if (transWas == null)
                    currentTransaction.close();
                currentTransaction = transWas;
            }
        }
	}

	private static void startIfNecessary() {
        if (currentTransaction == null) {
            if (!runningOnStartHooks) {
                runningOnStartHooks = true;
                try {
                    for (Runnable r : onStartHooks)
                        r.run();
                }
                finally {
                    runningOnStartHooks = false;
                }
            }
            currentTransaction = new Transaction();
        }
	}

	void prioritized(Node rank, Handler<Transaction> action) {
	    Entry e = new Entry(rank, action);
		prioritizedQ.add(e);
		entries.add(e);
	}

	/**
     * Add an action to run after all prioritized() actions.
     */
	void last(Runnable action) {
	    lastQ.add(action);
	}

	/**
     * Add an action to run after all last() actions.
     */
	void post_(int childIx, final Handler<Transaction> action) {
	    if (postQ == null)
	        postQ = new HashMap<Integer, Handler<Transaction>>();
	    // If an entry exists already, combine the old one with the new one.
	    final Handler<Transaction> existing = postQ.get(childIx);
	    Handler<Transaction> neu =
	        existing == null ? action
	                         : new Handler<Transaction>() {
                                   public void run(Transaction trans) {
                                       existing.run(trans);
                                       action.run(trans);
                                   }
                               };
	    postQ.put(childIx, neu);
	}

	/**
     * Execute the specified code after the current transaction is closed,
     * or immediately if there is no current transaction.
     */
	public static void post(final Runnable action) {
	    Transaction.run(new Handler<Transaction>() {
            public void run(Transaction trans) {
                // -1 will mean it runs before anything split/deferred, and will run
                // outside a transaction context.
                trans.post_(-1, new Handler<Transaction>() {
                    public void run(Transaction trans) {
                        action.run();
                    }
                });
            }
	    });
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

	void close() {
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
		    while (!postQ.isEmpty()) {
		        Iterator<Map.Entry<Integer, Handler<Transaction>>> iter = postQ.entrySet().iterator();
		        if (iter.hasNext()) {
		            Map.Entry<Integer, Handler<Transaction>> e = iter.next();
		            int ix = e.getKey();
                    Handler<Transaction> h = e.getValue();
                    iter.remove();
                    Transaction parent = currentTransaction;
                    try {
                        if (ix >= 0) {
                            Transaction trans = new Transaction();
                            currentTransaction = trans;
                            try {
                                h.run(trans);
                            } finally {
                                trans.close();
                            }
                        }
                        else {
                            currentTransaction = null;
                            h.run(null);
                        }
                    }
                    finally {
                        currentTransaction = parent;
                    }
		        }
		    }
		}
	}
}
