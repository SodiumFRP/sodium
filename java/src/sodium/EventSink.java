package sodium;

public class EventSink<A> extends Event<A> {
	public void send(A a) {
		Transaction.run((Transaction trans) -> { send(trans, a); });
	}
    void send(Transaction trans, A a) {
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
