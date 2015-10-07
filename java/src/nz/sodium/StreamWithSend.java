package nz.sodium;

import java.util.HashSet;

class StreamWithSend<A> extends Stream<A> {

	protected void send(Transaction trans, final A a) {
		if (firings.isEmpty())
			trans.last(new Runnable() {
				public void run() {
					firings.clear();
				}
			});
		firings.add(a);

		HashSet<Node.Target> listeners;
        synchronized (Transaction.listenersLock) {
            listeners = new HashSet<Node.Target>(node.listeners);
        }
		for (final Node.Target target : listeners) {
            trans.prioritized(target.node, new Handler<Transaction>() {
                public void run(Transaction trans2) {
                    Transaction.inCallback++;
                    try {  // Don't allow transactions to interfere with Sodium
                           // internals.
                        // Dereference the weak reference
                        TransactionHandler<Unit> uta = target.action.get();
                        if (uta != null)  // If it hasn't been gc'ed..., call it
                            ((TransactionHandler<A>)uta).run(trans2, a);
                    } catch (Throwable t) {
                        t.printStackTrace();
                    }
                    finally {
                        Transaction.inCallback--;
                    }
                }
            });
		}
	}

}
