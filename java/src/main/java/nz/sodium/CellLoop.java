package nz.sodium;

/**
 * A forward reference for a {@link Cell} equivalent to the Cell that is referenced. 
 */
public final class CellLoop<A> extends LazyCell<A> {
    public CellLoop() {
    	super(new StreamLoop<A>(), null);
    }

    /**
     * Resolve the loop to specify what the CellLoop was a forward reference to. It
     * must be invoked inside the same transaction as the place where the CellLoop is used.
     * This requires you to create an explicit transaction with {@link Transaction#run(Lambda0)}
     * or {@link Transaction#runVoid(Runnable)}.
     */
    public void loop(final Cell<A> a_out)
    {
        final CellLoop<A> me = this;
        Transaction.apply(new Lambda1<Transaction, Unit>() {
        	public Unit apply(final Transaction trans) {
                ((StreamLoop<A>)me.str).loop(a_out.updates());
                me.lazyInitValue = a_out.sampleLazy(trans);
                return Unit.UNIT;
            }
        });
    }

    @Override
    A sampleNoTrans()
    {
        if (!((StreamLoop<A>)str).assigned)
            throw new RuntimeException("CellLoop sampled before it was looped");
        return super.sampleNoTrans();
    }
}

