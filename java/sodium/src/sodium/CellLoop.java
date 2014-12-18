package sodium;

public final class CellLoop<A> extends Cell<A> {
    public CellLoop() {
    	super(new StreamLoop<A>(), null);
    }

    public void loop(Cell<A> a_out)
    {
        ((StreamLoop<A>)event).loop(a_out.updates());
        value = a_out.sample();
    }

    @Override
    protected A sampleNoTrans()
    {
        if (value == null)
            throw new RuntimeException("CellLoop sampled before it was looped");
        return value;
    }
}

