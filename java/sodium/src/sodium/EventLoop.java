package sodium;

import java.util.List;

public class EventLoop<A> extends Event<A> {
    private Event<A> ea_out;

    public EventLoop()
    {
    }

    // TO DO: Copy & paste from EventSink. Can we improve this?
    private void send(Transaction trans, A a) {
        if (firings.isEmpty())
            trans.last(new Runnable() {
            	public void run() { firings.clear(); }
            });
        firings.add(a);

        @SuppressWarnings("unchecked")
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

    public void loop(Event<A> ea_out)
    {
        if (this.ea_out != null)
            throw new RuntimeException("EventLoop looped more than once");
        this.ea_out = ea_out;
        final EventLoop<A> me = this;
        addCleanup(ea_out.listen_(this.node, new TransactionHandler<A>() {
            public void run(Transaction trans, A a) {
                me.send(trans, a);
            }
        }));
    }
}

