package sodium;

public final class CellLoop<A> extends LazyCell<A> {
    public CellLoop() {
    	super(new StreamLoop<A>(), null);
    }

    public void loop(Cell<A> a_out)
    {
        final CellLoop<A> me = this;
        Transaction.apply(new Lambda1<Transaction, Unit>() {
        	public Unit apply(final Transaction trans) {
                ((StreamLoop<A>)me.str).loop(a_out.updates(trans));
                me.lazyInitValue = a_out.sampleLazy(trans);
                return Unit.UNIT;
            }
        });
    }

    @Override
    protected A sampleNoTrans()
    {
        if (!((StreamLoop<A>)str).assigned)
            throw new RuntimeException("CellLoop sampled before it was looped");
        return super.sampleNoTrans();
    }
}

