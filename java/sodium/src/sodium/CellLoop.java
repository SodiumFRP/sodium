package sodium;

public final class CellLoop<A> extends LazyCell<A> {
    public CellLoop() {
    	super(new StreamLoop<A>(), null);
    }

    public void loop(Cell<A> a_out)
    {
        ((StreamLoop<A>)str).loop(a_out.updates());
        this.lazyInitValue = a_out.sampleLazy();
    }

    @Override
    protected A sampleNoTrans()
    {
        if (!((StreamLoop<A>)str).assigned)
            throw new RuntimeException("CellLoop sampled before it was looped");
        return super.sampleNoTrans();
    }
}

