package sodium;

import java.util.ArrayList;
import java.util.List;

public class Event<A> {
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

	protected final List<TransactionHandler<A>> listeners = new ArrayList<TransactionHandler<A>>();
	protected final List<Listener> finalizers = new ArrayList<Listener>();
	Node node = new Node(0L);

	public Event() {
	}

	public final Listener listen(final Handler<A> action) {
		return listen(Node.NULL, (Transaction trans, A a) -> { action.run(a); });
	}

	final Listener listen(Node target, TransactionHandler<A> action) {
		return Transaction.evaluate((Transaction trans) -> listen(target, trans, action));
	}

	Listener listen(Node target, Transaction trans, TransactionHandler<A> action) {
		node.linkTo(target);
		listeners.add(action);
		return new ListenerImplementation<A>(this, action, target);
	}

	public final <B> Event<B> map(Lambda1<A,B> f)
	{
		EventSink<B> out = new EventSink<B>();
		Listener l = listen(out.node, (Transaction trans, A a) -> {
			out.send(trans, f.evaluate(a));
		});
		return out.addCleanup(l);
	}

	protected final Event<A> addCleanup(Listener l) {
		finalizers.add(l);
		return this;
	}

	public final Behavior<A> hold(A initValue) {
		return new Behavior<A>(this, initValue);
	}

	public final <B> Event<B> snapshot(Behavior<B> beh)
	{
	    return snapshot(beh, (A a, B b) -> b);
	}

	public final <B,C> Event<C> snapshot(Behavior<B> b, Lambda2<A,B,C> f)
	{
		EventSink<C> out = new EventSink<C>();
		Listener l = listen(out.node, (Transaction trans, A a) -> {
			out.send(trans, f.evaluate(a, b.value));
		});
	    return out.addCleanup(l);
	}

	@Override
	protected void finalize() throws Throwable {
		for (Listener l : finalizers)
			l.unlisten();
	}
}
