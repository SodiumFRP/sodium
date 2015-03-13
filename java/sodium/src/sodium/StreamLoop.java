package sodium;

public class StreamLoop<A> extends StreamWithSend<A> {
    private Stream<A> ea_out;

    public StreamLoop()
    {
    	if (Transaction.getCurrentTransaction() == null)
    	    throw new RuntimeException("StreamLoop/CellLoop must be used within an explicit transaction");
    }

    public void loop(Stream<A> ea_out)
    {
        if (this.ea_out != null)
            throw new RuntimeException("StreamLoop looped more than once");
        this.ea_out = ea_out;
        final StreamLoop<A> me = this;
        unsafeAddCleanup(ea_out.listen_(this.node, new TransactionHandler<A>() {
            public void run(Transaction trans, A a) {
                me.send(trans, a);
            }
        }));
    }
}

