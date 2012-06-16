package sodium;

import java.util.ArrayList;
import java.util.List;

public abstract class Event<A> {
	private static final class ListenerImplementation<A> implements Listener {
		/**
		 * It's essential that we keep the listener alive while the caller holds
		 * the Listener, so that the finalizer doesn't get triggered.
		 */
		private final Event<A> event;
		private final TransactionHandler<A> action;
		private final Node target;

		private ListenerImplementation(Event<A> event, TransactionHandler<A> action, Node target) {
			this.event = event;
			this.action = action;
			this.target = target;
		}

		public void unlisten() {
			event.listeners.remove(action);
			event.node.unlinkTo(target);
		}

		protected void finalize() throws Throwable {
			unlisten();
		}
	}

	protected final ArrayList<TransactionHandler<A>> listeners = new ArrayList<TransactionHandler<A>>();
	protected final List<Listener> finalizers = new ArrayList<Listener>();
	Node node = new Node(0L);
	protected final List<A> firings = new ArrayList<A>();

	public Event() {
	}

	protected abstract Object[] sampleNow();

	/**
	 * Listen for firings of this event. The returned Listener has an unlisten()
	 * method to cause the listener to be removed. This is the observer pattern.
     */
	public final Listener listen(final Handler<A> action) {
		return listen_(Node.NULL, (Transaction trans2, A a) -> { action.run(a); });
	}

	final Listener listen_(Node target, TransactionHandler<A> action) {
		return Transaction.evaluate((Transaction trans1) ->
		    listen(target, trans1, action, false));
	}

	final Listener listen(Node target, Transaction trans, TransactionHandler<A> action, boolean suppressEarlierFirings) {
		if (node.linkTo(target))
		    trans.toRegen = true;
		Object[] aNow = sampleNow();
		if (aNow != null) {    // In cases like values(), we start with an initial value.
		    for (int i = 0; i < aNow.length; i++)
                action.run(trans, (A)aNow[i]);
        }
		listeners.add(action);
		if (!suppressEarlierFirings) {
            // Anything sent already in this transaction must be sent now so that
            // there's no order dependency between send and listen.
            for (A a : firings)
                action.run(trans, a);
        }
		return new ListenerImplementation<A>(this, action, target);
	}

    /**
     * Transform the event's value according to the supplied function.
     */
	public final <B> Event<B> map(final Lambda1<A,B> f)
	{
	    final Event<A> ev = this;
	    final EventSink<B> out = new EventSink<B>() {
    		@Override
            protected Object[] sampleNow()
            {
                Object[] oi = ev.sampleNow();
                if (oi != null) {
                    Object[] oo = new Object[oi.length];
                    for (int i = 0; i < oo.length; i++)
                        oo[i] = f.evaluate((A)oi[i]);
                    return oo;
                }
                else
                    return null;
            }
	    };
        Listener l = listen_(out.node, (Transaction trans2, A a) -> {
            out.send(trans2, f.evaluate(a));
        });
        return out.addCleanup(l);
	}

	/**
	 * Create a behavior with the specified initial value, that gets updated
     * by the values coming through the event. The 'current value' of the behavior
     * is notionally the value as it was 'at the start of the transaction'.
     * That is, state updates caused by event firings get processed at the end of
     * the transaction.
     */
	public final Behavior<A> hold(A initValue) {
		return Transaction.evaluate((Transaction trans) ->
		    new Behavior<A>(lastFiringOnly(trans), initValue));
	}

	/**
	 * Variant of snapshot that throws away the event's value and captures the behavior's.
	 */
	public final <B> Event<B> snapshot(Behavior<B> beh)
	{
	    return snapshot(beh, (A a, B b) -> b);
	}

	/**
	 * Sample the behavior at the time of the event firing. Note that the 'current value'
     * of the behavior that's sampled is the value as at the start of the transaction
     * before any state changes of the current transaction are applied through 'hold's.
     */
	public final <B,C> Event<C> snapshot(final Behavior<B> b, final Lambda2<A,B,C> f)
	{
	    final Event<A> ev = this;
		EventSink<C> out = new EventSink<C>() {
    		@Override
            protected Object[] sampleNow()
            {
                Object[] oi = ev.sampleNow();
                if (oi != null) {
                    Object[] oo = new Object[oi.length];
                    for (int i = 0; i < oo.length; i++)
                        oo[i] = f.evaluate((A)oi[i], b.value);
                    return oo;
                }
                else
                    return null;
            }
		};
        Listener l = listen_(out.node, (Transaction trans2, A a) -> {
            out.send(trans2, f.evaluate(a, b.value));
        });
        return out.addCleanup(l);
	}

    /**
     * Merge two streams of events of the same type.
     *
     * In the case where two event occurrences are simultaneous (i.e. both
     * within the same transaction), both will be delivered in the same
     * transaction. If the event firings are ordered for some reason, then
     * their ordering is retained. In many common cases the ordering will
     * be undefined.
     */
	public static <A> Event<A> merge(final Event<A> ea, final Event<A> eb)
	{
	    EventSink<A> out = new EventSink<A>() {
    		@Override
            protected Object[] sampleNow()
            {
                Object[] oa = ea.sampleNow();
                Object[] ob = eb.sampleNow();
                if (oa != null && ob != null) {
                    Object[] oo = new Object[oa.length + ob.length];
                    int j = 0;
                    for (int i = 0; i < oa.length; i++) oo[j++] = oa[i];
                    for (int i = 0; i < ob.length; i++) oo[j++] = ob[i];
                    return oo;
                }
                else
                if (oa != null)
                    return oa;
                else
                    return ob;
            }
	    };
        TransactionHandler<A> h = (Transaction trans, A a) -> {
            out.send(trans, a);
        };
        Listener l1 = ea.listen_(out.node, h);
        Listener l2 = eb.listen_(out.node, h);
        return out.addCleanup(l1).addCleanup(l2);
	}

    /**
     * If there's more than one firing in a single transaction, combine them into
     * one using the specified combining function.
     *
     * If the event firings are ordered, then the first will appear at the left
     * input of the combining function. In most common cases it's best not to
     * make any assumptions about the ordering, and the combining function would
     * ideally be commutative.
     */
	public final Event<A> coalesce(final Lambda2<A,A,A> f)
	{
	    return Transaction.evaluate((Transaction trans) -> coalesce(trans, f));
	}

	final Event<A> coalesce(Transaction trans1, final Lambda2<A,A,A> f)
	{
	    final Event<A> ev = this;
	    final EventSink<A> out = new EventSink<A>() {
    		@Override
            protected Object[] sampleNow()
            {
                Object[] oi = ev.sampleNow();
                if (oi != null) {
                    A o = (A)oi[0];
                    for (int i = 1; i < oi.length; i++)
                        o = f.evaluate(o, (A)oi[i]);
                    return new Object[] { o };
                }
                else
                    return null;
            }
	    };
        TransactionHandler<A> h = new TransactionHandler<A>() {
            private boolean accumValid = false;
            private A accum;
            @Override
            public void run(Transaction trans1, A a) {
                if (accumValid)
                    accum = f.evaluate(accum, a);
                else {
                    trans1.prioritized(out.node, (Transaction trans2) -> {
                        out.send(trans2, this.accum);
                        this.accumValid = false;
                        this.accum = null;
                    });
                    accum = a;
                    accumValid = true;
                }
            }
        };

        Listener l = listen(out.node, trans1, h, false);
        return out.addCleanup(l);
    }

    /**
     * Clean up the output by discarding any firing other than the last one. 
     */
    final Event<A> lastFiringOnly(Transaction trans)
    {
        return coalesce(trans, (A first, A second) -> second);
    }

    /**
     * Merge two streams of events of the same type, combining simultaneous
     * event occurrences.
     *
     * In the case where multiple event occurrences are simultaneous (i.e. all
     * within the same transaction), they are combined using the same logic as
     * 'coalesce'.
     */
    public static <A> Event<A> mergeWith(Lambda2<A,A,A> f, Event<A> ea, Event<A> eb)
    {
        return merge(ea, eb).coalesce(f);
    }

    /**
     * Only keep event occurrences for which the predicate returns true.
     */
    public final Event<A> filter(final Lambda1<A,Boolean> f)
    {
        final Event<A> ev = this;
        EventSink<A> out = new EventSink<A>() {
    		@Override
            protected Object[] sampleNow()
            {
                Object[] oi = ev.sampleNow();
                if (oi != null) {
                    Object[] oo = new Object[oi.length];
                    int j = 0;
                    for (int i = 0; i < oi.length; i++)
                        if (f.evaluate((A)oi[i]))
                            oo[j++] = oi[i];
                    if (j < oo.length) {
                        Object[] oo2 = new Object[j];
                        for (int i = 0; i < j; i++)
                            oo2[i] = oo[i];
                        oo = oo2;
                    }
                    return oo;
                }
                else
                    return null;
            }
        };
        Listener l = listen_(out.node, (Transaction trans2, A a) -> {
            if (f.evaluate(a)) out.send(trans2, a);
        });
        return out.addCleanup(l);
    }

    /**
     * Filter out any event occurrences whose value is a Java null pointer.
     */
    public final Event<A> filterNotNull()
    {
        return filter((A a) -> a != null);
    }

	/**
	 * Loop an event round so it can effectively be forward referenced.
	 * This adds a cycle to your graph of relationships between events and behaviors.
	 */
    public static <A,B> B loop(Lambda1<Event<A>,Tuple2<B,Event<A>>> f)
    {
        EventSink<A> ea_in = new EventSink();
        Tuple2<B,Event<A>> b_ea = f.evaluate(ea_in);
        B b = b_ea.a;
        Event<A> ea_out = b_ea.b;
        Listener l = ea_out.listen_(ea_in.node, (Transaction trans, A a) -> {
            ea_in.send(trans, a);
        });
        ea_in.addCleanup(l);
        return b;
    }

    /**
     * Let event occurrences through only when the behavior's value is True.
     * Note that the behavior's value is as it was at the start of the transaction,
     * that is, no state changes from the current transaction are taken into account.
     */
    public final Event<A> gate(Behavior<Boolean> bPred)
    {
        return snapshot(bPred, (A a, Boolean pred) -> pred ? a : null).filterNotNull();
    }

    /**
     * Transform an event with a generalized state loop (a mealy machine). The function
     * is passed the input and the old state and returns the new state and output value.
     */
    public final <B,S> Event<B> collect(final S initState, final Lambda2<A, S, Tuple2<B, S>> f)
    {
        final Event<A> ea = this;
        return Event.loop(
            new Lambda1<Event<S>, Tuple2<Event<B>,Event<S>>>() {
                public Tuple2<Event<B>,Event<S>> evaluate(Event<S> es) {
                    Behavior<S> s = es.hold(initState);
                    Event<Tuple2<B,S>> ebs = ea.snapshot(s, f);
                    Event<B> eb = ebs.map(bs -> bs.a);
                    Event<S> es_out = ebs.map(bs -> bs.b);
                    return new Tuple2<Event<B>,Event<S>>(eb, es_out);
                }
            }
        );
    }

    /**
     * Accumulate on input event, outputting the new state each time.
     */
    public final <S> Event<S> accum(final S initState, final Lambda2<A, S, S> f)
    {
        final Event<A> ea = this;
        return Event.loop(
            new Lambda1<Event<S>, Tuple2<Event<S>,Event<S>>>() {
                public Tuple2<Event<S>,Event<S>> evaluate(Event<S> es) {
                    Behavior<S> s = es.hold(initState);
                    Event<S> es_out = ea.snapshot(s, f);
                    return new Tuple2<Event<S>,Event<S>>(es_out, es_out);
                }
            }
        );
    }

    /**
     * Count event occurrences, starting with 1 for the first occurrence.
     */
    public final Event<Integer> countE()
    {
        return map((A a) -> 1).accum(0, (a,b)->a+b);
    }

    /**
     * Count event occurrences, giving a behavior that starts with 0 before the first occurrence.
     */
    public final Behavior<Integer> count()
    {
        return countE().hold(0);
    }

    /**
     * Throw away all event occurrences except for the first one.
     */
    public final Event<A> once()
    {
        // This is a bit long-winded but it's efficient because it deregisters
        // the listener.
        final Event<A> ev = this;
        final Listener[] la = new Listener[1];
        EventSink<A> out = new EventSink<A>() {
            @Override
            protected Object[] sampleNow()
            {
                Object[] oi = ev.sampleNow();
                Object[] oo = oi;
                if (oo != null) {
                    if (oo.length > 1)
                        oo = new Object[] { oi[0] };
                    if (la[0] != null) {
                        la[0].unlisten();
                        la[0] = null;
                    }
                }
                return oo;
            }
        };
        la[0] = ev.listen_(out.node, (Transaction trans, A a) -> {
            out.send(trans, a);
            if (la[0] != null) {
                la[0].unlisten();
                la[0] = null;
            }
        });
        return out.addCleanup(la[0]);
    }

    Event<A> addCleanup(Listener cleanup)
    {
        finalizers.add(cleanup);
        return this;
    }

	@Override
	protected void finalize() throws Throwable {
		for (Listener l : finalizers)
			l.unlisten();
	}
}

