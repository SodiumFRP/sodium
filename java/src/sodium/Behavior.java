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
    			action.run(trans, value);
    		    return changes().listen(target, trans, action);
    		}
    	};
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
		final EventSink<B> out = new EventSink<B>();

		Handler<Transaction> h = new Handler<Transaction>() {
			boolean fired = false;			
			@Override
			public void run(Transaction trans1) {
				if (fired) 
					return;
				
				fired = true;
				trans1.prioritized(out.node, (Transaction trans2) -> {
				   out.send(trans2, bf.newValue().evaluate(ba.newValue()));
			       fired = false;
				});
			}
		};
	
		Listener l1 = bf.changes().listen(out.node, (Transaction trans1, Lambda1<A,B> f) -> {
			h.run(trans1);
		});
		Listener l2 = ba.changes().listen(out.node, (Transaction trans1, A a) -> {
			h.run(trans1);
		});
		return out.addCleanup(l1).addCleanup(l2).hold(bf.value.evaluate(ba.value));
	}

	@Override
	protected void finalize() throws Throwable {
		cleanup.unlisten();
	}
}
