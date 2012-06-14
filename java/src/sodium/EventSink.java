package sodium;

import java.util.List;

public class EventSink<A> extends Event<A> {
    public EventSink() {}
	public void send(A a) {
		Transaction.run((Transaction trans) -> { send(trans, a); });
	}
    void send(Transaction trans, A a) {
        if (firings.isEmpty())
            trans.last(() -> { firings.clear(); });
        firings.add(a);
        
        List<TransactionHandler<A>> listeners = (List<TransactionHandler<A>>)this.listeners.clone();
    	for (TransactionHandler<A> action : listeners) {
    		try {
                action.run(trans, a);
    		}
    		catch (Throwable t) {
    		    t.printStackTrace();
    		}
    	}
    }
}
