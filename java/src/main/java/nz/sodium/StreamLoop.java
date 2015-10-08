package nz.sodium;

/**
 * A forward reference for a {@link Stream} equivalent to the Stream that is referenced. 
 */
public class StreamLoop<A> extends StreamWithSend<A> {
    boolean assigned = false;

    public StreamLoop()
    {
    	if (Transaction.getCurrentTransaction() == null)
    	    throw new RuntimeException("StreamLoop/CellLoop must be used within an explicit transaction");
    }

    /**
     * Resolve the loop to specify what the StreamLoop was a forward reference to. It
     * must be invoked inside the same transaction as the place where the StreamLoop is used.
     * This requires you to create an explicit transaction with {@link Transaction#run(Lambda0)}
     * or {@link Transaction#runVoid(Runnable)}.
     */
    public void loop(final Stream<A> ea_out)
    {
        if (assigned)
            throw new RuntimeException("StreamLoop looped more than once");
        assigned = true;
        final StreamLoop<A> me = this;
        Transaction.runVoid(new Runnable() {
            public void run() {
                unsafeAddCleanup(ea_out.listen_(StreamLoop.this.node, new TransactionHandler<A>() {
                    public void run(Transaction trans, A a) {
                        me.send(trans, a);
                    }
                }));
            }
        });
    }
}

