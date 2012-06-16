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

	public final Behavior<A> hold(A initValue) {
		return Transaction.evaluate((Transaction trans) ->
		    new Behavior<A>(lastFiringOnly(trans), initValue));
	}

	public final <B> Event<B> snapshot(Behavior<B> beh)
	{
	    return snapshot(beh, (A a, B b) -> b);
	}

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

    public static <A> Event<A> mergeWith(Lambda2<A,A,A> f, Event<A> ea, Event<A> eb)
    {
        return merge(ea, eb).coalesce(f);
    }

    public Event<A> filter(final Lambda1<A,Boolean> f)
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

    public Event<A> filterNotNull()
    {
        return filter((A a) -> a != null);
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

