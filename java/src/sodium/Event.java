package sodium;

import java.util.ArrayList;
import java.util.List;

public class Event<A> {
	private static final class ListenerImplementation<A> extends Listener {
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

	public final Listener listen(final Handler<A> action) {
		return Transaction.evaluate((Transaction trans1) ->
		    listen(Node.NULL, trans1, (Transaction trans2, A a) -> { action.run(a); }));
	}

	Listener listen(Node target, Transaction trans, TransactionHandler<A> action) {
		if (node.linkTo(target))
		    trans.toRegen = true;
		listeners.add(action);
		// Anything sent already in this transaction must be sent now so that
		// there's no order dependency between send and listen.
		for (A a : firings)
		    action.run(trans, a);
		return new ListenerImplementation<A>(this, action, target);
	}

	public final <B> Event<B> map(final Lambda1<A,B> f)
	{
	    final Event<A> ev = this;
		return new EventSink<B>() {
            Listener listen(Node target, Transaction trans1, TransactionHandler<B> action) {
                final EventSink<B> out = this;
                Listener l = ev.listen(out.node, trans1, (Transaction trans2, A a) -> {
                    out.send(trans2, f.evaluate(a));
                });
                return super.listen(target, trans1, action).addCleanup(l);
            }
        };
	}

	public final Behavior<A> hold(A initValue) {
		return new Behavior<A>(lastFiringOnly(), initValue);
	}

	public final <B> Event<B> snapshot(Behavior<B> beh)
	{
	    return snapshot(beh, (A a, B b) -> b);
	}

	public final <B,C> Event<C> snapshot(final Behavior<B> b, final Lambda2<A,B,C> f)
	{
	    final Event<A> ev = this;
		return new EventSink<C>() {
		    Listener listen(Node target, Transaction trans1, TransactionHandler<C> action) {
		        final EventSink<C> out = this;
                Listener l = ev.listen(out.node, trans1, (Transaction trans2, A a) -> {
                    out.send(trans2, f.evaluate(a, b.value));
                });
                return super.listen(target, trans1, action).addCleanup(l);
            }
        };
	}

	public static <A> Event<A> merge(final Event<A> ea, final Event<A> bb)
	{
	    return new EventSink<A>() {
	        Listener listen(Node target, Transaction trans1, TransactionHandler<A> action) {
		        final EventSink<A> out = this;
                TransactionHandler<A> h = (Transaction trans, A a) -> {
                    out.send(trans, a);
                };
                Listener l1 = ea.listen(out.node, trans1, h);
                Listener l2 = bb.listen(out.node, trans1, h);
                return super.listen(target, trans1, action).addCleanup(l1).addCleanup(l2);
            }
        };
	}

	public final Event<A> coalesce(final Lambda2<A,A,A> f)
	{
	    final Event<A> ev = this;
	    return new EventSink<A>() {
            Listener listen(Node target, Transaction trans, TransactionHandler<A> action) {
                final EventSink<A> out = this;
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

                Listener l = ev.listen(out.node, trans, h);
                return super.listen(target, trans, action).addCleanup(l);
            }
        };
    }

    /**
     * Clean up the output by discarding any firing other than the last one. 
     */
    Event<A> lastFiringOnly()
    {
        return coalesce((A first, A second) -> second);
    }

    public static <A> Event<A> mergeWith(Lambda2<A,A,A> f, Event<A> ea, Event<A> eb)
    {
        return merge(ea, eb).coalesce(f);
    }

    public Event<A> filter(final Lambda1<A,Boolean> f)
    {
        final Event<A> ev = this;
        return new EventSink<A>() {
            Listener listen(Node target, Transaction trans1, TransactionHandler<A> action) {
                final EventSink<A> out = this;
                Listener l = ev.listen(out.node, trans1, (Transaction trans2, A a) -> {
                    if (f.evaluate(a)) out.send(trans2, a);
                });
                return super.listen(target, trans1, action).addCleanup(l);
            }
        };
    }

    public Event<A> filterNotNull()
    {
        final Event<A> ev = this;
        return new EventSink<A>() {
            Listener listen(Node target, Transaction trans1, TransactionHandler<A> action) {
                final EventSink<A> out = this;
                Listener l = ev.listen(out.node, trans1, (Transaction trans2, A a) -> {
                    if (a != null) out.send(trans2, a);
                });
                return super.listen(target, trans1, action).addCleanup(l);
            }
        };
    }
}

