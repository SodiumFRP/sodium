package sodium;

import java.util.List;

public class StreamLoop<A> extends Stream<A> {
    private Stream<A> ea_out;

    public StreamLoop()
    {
    	if (Transaction.getCurrentTransaction() == null)
    	    throw new RuntimeException("StreamLoop/CellLoop must be used within an explicit transaction");
    }

	protected Object[] sampleNow() {
	    if (ea_out == null)
            throw new RuntimeException("StreamLoop sampled before it was looped");
        return ea_out.sampleNow();
	}

    // TO DO: Copy & paste from StreamSink. Can we improve this?
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

    public void loop(Stream<A> ea_out)
    {
        if (this.ea_out != null)
            throw new RuntimeException("StreamLoop looped more than once");
        this.ea_out = ea_out;
        final StreamLoop<A> me = this;
        addCleanup(ea_out.listen_(this.node, new TransactionHandler<A>() {
            public void run(Transaction trans, A a) {
                me.send(trans, a);
            }
        }));
    }
}

