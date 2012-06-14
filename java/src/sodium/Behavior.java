package sodium;

public class Behavior<A> {
	protected Event<A> event;
	A value;
	A valueUpdate;
	private Listener cleanup;

    Behavior(Event<A> event, A initValue)
    {
    	this.event = event;
    	this.value = initValue;
    	Transaction.run((Transaction trans1) -> {
    		this.cleanup = event.listen(Node.NULL, trans1, (Transaction trans2, A a) -> {
	    		if (this.valueUpdate == null) {
	    			trans2.last(() -> {
	    				this.value = this.valueUpdate;
	    				this.valueUpdate = null;
	    			});
	    		}
	    		this.valueUpdate = a;
	    	});
    	});
    }

    /**
     * @return The value including any updates that have happened in this transaction.
     */
    A newValue()
    {
    	return valueUpdate == null ? value :  valueUpdate;
    }

    public Event<A> changes()
    {
    	return event;
    }

    public Event<A> values()
    {
    	return new Event<A>() {
    		@Override
    		public Listener listen(Node target, Transaction trans, TransactionHandler<A> action) {
    			action.run(trans, value);  // Start with the initial value.
    		    return changes().listen(target, trans, action);
    		}
    	}.coalesce((A first, A second) -> second);
    }

	public final <B> Behavior<B> map(Lambda1<A,B> f)
	{
		return changes().map(f).hold(f.evaluate(value));
	}
	
	public final <B,C> Behavior<C> lift(Lambda2<A,B,C> f, Behavior<B> b)
	{
		Lambda1<A, Lambda1<B,C>> ffa = (A aa) -> (B bb) -> f.evaluate(aa,bb);
		Behavior<Lambda1<B,C>> bf = map(ffa);
		return apply(bf, b);
	}

	public static final <A,B,C> Behavior<C> lift(Lambda2<A,B,C> f, Behavior<A> a, Behavior<B> b)
	{
		return a.lift(f, b);
	}

	public static <A,B> Behavior<B> apply(final Behavior<Lambda1<A,B>> bf, final Behavior<A> ba)
	{
		final EventSink<B> out0 = new EventSink<B>() {
		    Listener listen(Node target, Transaction trans1, TransactionHandler<B> action) {
		        final EventSink<B> out = this;
                Handler<Transaction> h = new Handler<Transaction>() {
                    boolean fired = false;			
                    @Override
                    public void run(Transaction trans2) {
                        if (fired) 
                            return;
                        
                        fired = true;
                        trans2.prioritized(out.node, (Transaction trans3) -> {
                           out.send(trans3, bf.newValue().evaluate(ba.newValue()));
                           fired = false;
                        });
                    }
                };
            
                Listener l1 = bf.changes().listen(out.node, trans1, (Transaction trans2, Lambda1<A,B> f) -> {
                    h.run(trans2);
                });
                Listener l2 = ba.changes().listen(out.node, trans1, (Transaction trans2, A a) -> {
                    h.run(trans2);
                });
                return super.listen(target, trans1, action).addCleanup(l1).addCleanup(l2);
            }
        };
        return out0.hold(bf.value.evaluate(ba.value));
	}

	public static <A> Behavior<A> switchB(final Behavior<Behavior<A>> bba)
	{
	    A za = bba.value.value;
	    final EventSink<A> out0 = new EventSink<A>() {
	        Listener listen(Node target, Transaction trans1, TransactionHandler<A> action) {
		        final EventSink<A> out = this;
                TransactionHandler<Behavior<A>> h = new TransactionHandler<Behavior<A>>() {
                    private Listener currentListener;
                    @Override
                    public void run(Transaction trans2, Behavior<A> ba) {
                        if (currentListener != null)
                            currentListener.unlisten();
                        currentListener = ba.values().listen(out.node, trans2, (Transaction trans3, A a) -> {
                            out.send(trans3, a);
                        });
                    }
        
                    @Override
                    protected void finalize() throws Throwable {
                        if (currentListener != null)
                            currentListener.unlisten();
                    }
                };
                Listener l1 = bba.values().listen(out.node, trans1, h);
                return super.listen(target, trans1, action).addCleanup(l1);
            }
        };
        return out0.hold(za);
	}

	@Override
	protected void finalize() throws Throwable {
		cleanup.unlisten();
	}
}
