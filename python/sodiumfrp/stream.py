from dataclasses import dataclass
from threading import RLock
import traceback
from typing import \
    Any, \
    Callable, \
    Generic, \
    List, \
    Optional, \
    Sequence, \
    Set, \
    Tuple, \
    TypeVar

from sodiumfrp.lazy import Lazy
from sodiumfrp.listener import Listener
from sodiumfrp.node import NODE_NULL, Node, Target
from sodiumfrp.transaction import Transaction
from sodiumfrp.typing import Handler, TransactionHandler
from sodiumfrp.unit import Unit, UNIT

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")
S = TypeVar("S")


#     package nz.sodium;
# 
# import java.util.ArrayList;
# import java.util.List;
# import java.util.HashSet;
# import java.util.Optional;
# import java.util.Vector;
# 
# /**
#  * Represents a stream of discrete events/firings containing values of type A.
#  */
# public class Stream<A> {

class Stream(Generic[A]):
    """
    Represents a stream of discrete events/firings containing values of
    type A.
    """
# 
#     static HashSet<Listener> keepListenersAlive = new HashSet<Listener>();
    _keep_listeners_alive: Set[Listener] = set()
    _keep_listeners_alive_lock: RLock = RLock()
# 
# 	final Node node;
# 	final List<Listener> finalizers;
# 	final List<A> firings;
# 
# 	private Stream(Node node, List<Listener> finalizers, List<A> firings) {
    def __init__(self,
            node: Node,
            finalizers: List[Listener],
            firings: List[A]) -> None:
        self._node = node
        self._finalizers = finalizers
        self._firings = firings
# 	    this.node = node;
# 	    this.finalizers = finalizers;
#         this.firings = firings;
# 	}

# 	/**
# 	 * A stream that never fires.
# 	 */
# 	public Stream() {
    @staticmethod
    def never() -> "StreamWithSend[A]":
        """ A stream that never fires. """
        return StreamWithSend()
# 	    this.node = new Node(0L);
# 	    this.finalizers = new ArrayList<Listener>();
# 	    this.firings = new ArrayList<A>();
# 	}
# 
# 
# 	/**
# 	 * Listen for events/firings on this stream. This is the observer pattern. The
# 	 * returned {@link Listener} has a {@link Listener#unlisten()} method to cause the
# 	 * listener to be removed. This is an OPERATIONAL mechanism is for interfacing between
# 	 * the world of I/O and for FRP.
# 	 * @param handler The handler to execute when there's a new value.
# 	 *   You should make no assumptions about what thread you are called on, and the
# 	 *   handler should not block. You are not allowed to use {@link CellSink#send(Object)}
# 	 *   or {@link StreamSink#send(Object)} in the handler.
# 	 *   An exception will be thrown, because you are not meant to use this to create
# 	 *   your own primitives.
#      */
# 	public final Listener listen(final Handler<A> handler) {
    def listen(self, handler: Handler[A]) -> Listener:
        """
        Listen for events/firings on this stream. This is the observer
        pattern. The returned `Listener` has a `Listener.unlisten()` method
        to cause the listener to be removed. This is an OPERATIONAL
        mechanism is for interfacing between the world of I/O and for FRP.

        @param handler The handler to execute when there's a new value.
            You should make no assumptions about what thread you are
            called on, and the handler should not block. You are not allowed
            to use `CellSink.send()` or `StreamSink.send()` in the handler.
            An exception will be thrown, because you are not meant to use
            this to create your own primitives.
        """
        l0 = self.listen_weak(handler)
        def unlisten(_self: Listener) -> None:
            l0.unlisten()
            with Stream._keep_listeners_alive_lock:
                Stream._keep_listeners_alive.remove(_self)
        l = Listener(unlisten)
        with Stream._keep_listeners_alive_lock:
            Stream._keep_listeners_alive.add(l)
        return l
#         final Listener l0 = listenWeak(handler);
#         Listener l = new Listener() {
#             public void unlisten() {
#                 l0.unlisten();
#                 synchronized (keepListenersAlive) {
#                     keepListenersAlive.remove(this);
#                 }
#             }
#         };
#         synchronized (keepListenersAlive) {
#             keepListenersAlive.add(l);
#         }
#         return l;
# 	}
# 
#     /**
#      * A variant of {@link #listen(Handler)} that handles the first event and then
#      * automatically deregisters itself. This is useful for implementing things that
#      * work like promises.
#      */
#     public final Listener listenOnce(final Handler<A> handler) {
#         final Listener[] lRef = new Listener[1];
#         lRef[0] = listen(new Handler<A>() {
#             public void run(A a) {
#                 lRef[0].unlisten();
#                 handler.run(a);
#             }
#         });
#         return lRef[0];
#     }
# 
# 	final Listener listen_(final Node target, final TransactionHandler<A> action) {
    def _listen(self,
            target: Node,
            action: TransactionHandler[A]) -> Listener:
        return Transaction._apply(
            lambda trans1: self._listen_internal(
                target, trans1, action, False))
# 		return Transaction.apply(new Lambda1<Transaction, Listener>() {
# 			public Listener apply(Transaction trans1) {
# 				return listen(target, trans1, action, false);
# 			}
# 		});
# 	}
# 
#     /**
#      * A variant of {@link #listen(Handler)} that will deregister the listener automatically
#      * if the listener is garbage collected. With {@link #listen(Handler)}, the listener is
#      * only deregistered if {@link Listener#unlisten()} is called explicitly.
#      * <P>
#      * This method should be used for listeners that are to be passed to {@link Stream#addCleanup(Listener)}
#      * to ensure that things don't get kept alive when they shouldn't.
#      */
#     public final Listener listenWeak(final Handler<A> action) {
    def listen_weak(self, action: Handler[A]) -> Listener:
        """
        A variant of `listen(Handler)` that will deregister the listener
        automatically if the listener is garbage collected. With
        `listen(Handler)`, the listener is only deregistered if
        `Listener.unlisten()` is called explicitly.

        This method should be used for listeners that are to be passed to
        `Stream.add_cleanup_listener()` to ensure that things don't get
        kept alive when they shouldn't.
        """
        return self._listen(NODE_NULL, lambda trans2, a: action(a))
# 		return listen_(Node.NULL, new TransactionHandler<A>() {
# 			public void run(Transaction trans2, A a) {
# 				action.run(a);
# 			}
# 		});
#     }
# 
# 	@SuppressWarnings("unchecked")
# 	final Listener listen(Node target, Transaction trans, final TransactionHandler<A> action, boolean suppressEarlierFirings) {
    def _listen_internal(self,
            target: Node,
            trans: Transaction,
            action: TransactionHandler[A],
            suppress_earlier_firings: bool) -> Listener:
        node_target_: List[Target] = [None]
        with Transaction._listeners_lock:
            if self._node._link_to(action, target, node_target_):
                trans._to_regen = True
# 	    Node.Target[] node_target_ = new Node.Target[1];
#         synchronized (Transaction.listenersLock) {
#             if (node.linkTo((TransactionHandler<Unit>)action, target, node_target_))
#                 trans.toRegen = true;
#         }
        node_target = node_target_[0]
        firings = self._firings.copy()
#         Node.Target node_target = node_target_[0];
#         final List<A> firings = new ArrayList<A>(this.firings);
        if (not suppress_earlier_firings) and (len(firings) > 0):
            def handler(trans2: Optional[Transaction]) -> None:
                for firing in firings:
                    Transaction.in_callback += 1
                    try:
                        action(trans2, firing)
                    except Exception as e:
                        # Don't allow transactions to interfere with Sodium
                        # internals.
                        traceback.print_exc()
                    finally:
                        Transaction.in_callback -= 1
            trans._prioritized(target, handler)
#         if (!suppressEarlierFirings && !firings.isEmpty())
#             trans.prioritized(target, new Handler<Transaction>() {
#                 public void run(Transaction trans2) {
#                     // Anything sent already in this transaction must be sent now so that
#                     // there's no order dependency between send and listen.
#                     for (A a : firings) {
#                         Transaction.inCallback++;
#                         try {  // Don't allow transactions to interfere with Sodium
#                                // internals.
#                             action.run(trans2, a);
#                         } catch (Throwable t) {
#                             t.printStackTrace();
#                         }
#                         finally {
#                             Transaction.inCallback--;
#                         }
#                     }
#                 }
#             });

# 	private static final class ListenerImplementation<A> extends Listener {
        captures: List[Any] = [
            # It's essential that we keep the listener alive while
            # the caller holds the Listener, so that the finalizer doesn't
            # get triggered.
            self,
            # It's also essential that we keep the action alive, since
            # the node uses a weak reference.
            action,
            node_target
        ]
        # 		/**
        # 		 * It's essential that we keep the listener alive while the caller holds
        # 		 * the Listener, so that the finalizer doesn't get triggered.
        # 		 */
        # 		private Stream<A> event;
        # 		/**
        # 		 * It's also essential that we keep the action alive, since the node uses
        # 		 * a weak reference.
        # 		 */
        # 		private TransactionHandler<A> action;
        # 		private Node.Target target;
        # 
        # 		private ListenerImplementation(Stream<A> event, TransactionHandler<A> action, Node.Target target) {
        # 			this.event = event;
        # 			this.action = action;
        # 			this.target = target;
        # 		}
        # 
        # 		public void unlisten() {
        def unlisten(_self: Listener) -> None:
            event, _, target = captures
            with Transaction._listeners_lock:
                if event is not None:
                    event._node._unlink_to(target)
                    # Release captured references
                    captures[0] = None
                    captures[1] = None
                    captures[2] = None
# 		    synchronized (Transaction.listenersLock) {
# 		        if (this.event != null) {
#                     event.node.unlinkTo(target);
#                     this.event = null;
#                     this.action = null;
#                     this.target = null;
#                 }
#             }
# 		}
# 	}

        return Listener(unlisten)
# 		return new ListenerImplementation<A>(this, action, node_target);
# 	}
# 
#     /**
#      * Transform the stream's event values according to the supplied function, so the returned
#      * Stream's event values reflect the value of the function applied to the input
#      * Stream's event values.
#      * @param f Function to apply to convert the values. It may construct FRP logic or use
#      *    {@link Cell#sample()} in which case it is equivalent to {@link Stream#snapshot(Cell)}ing the
#      *    cell. Apart from this the function must be <em>referentially transparent</em>.
#      */
# 	public final <B> Stream<B> map(final Lambda1<A,B> f)
    def map(self, f: Callable[[A], B]) -> "Stream[B]":
        """
        Transform the stream's event values according to the supplied
        function, so the returned Stream's event values reflect the value
        of the function applied to the input Stream's event values.

        @param f Function to apply to convert the values. It may construct
            FRP logic or use `Cell.sample()` in which case it is equivalent
            to `Stream.snapshot(Cell)`ing the cell. Apart from this
            the function must be **referentially transparent**.
        """
        out: StreamWithSend[B] = StreamWithSend()
        l = self._listen(out._node,
            lambda trans2, a: out._send(trans2, f(a)))
        return out._unsafe_add_cleanup(l)
# 	{
# 	    final Stream<A> ev = this;
# 	    final StreamWithSend<B> out = new StreamWithSend<B>();
#         Listener l = listen_(out.node, new TransactionHandler<A>() {
#         	public void run(Transaction trans2, A a) {
# 	            out.send(trans2, f.apply(a));
# 	        }
#         });
#         return out.unsafeAddCleanup(l);
# 	}
# 
#     /**
#      * Transform the stream's event values into the specified constant value.
#      * @param b Constant value.
#      */
# 	public final <B> Stream<B> mapTo(final B b)
    def map_to(self, b: B) -> "Stream[B]":
        return self.map(lambda _: b)
# 	{
# 		return this.<B>map(new Lambda1<A, B>() {
# 			public B apply(A a) {
# 			    return b;
# 			}
# 		});
# 	}
# 
# 	/**
# 	 * Create a {@link Cell} with the specified initial value, that is updated
#      * by this stream's event values.
#      * <p>
#      * There is an implicit delay: State updates caused by event firings don't become
#      * visible as the cell's current value as viewed by {@link Stream#snapshot(Cell, Lambda2)}
#      * until the following transaction. To put this another way,
#      * {@link Stream#snapshot(Cell, Lambda2)} always sees the value of a cell as it was before
#      * any state changes from the current transaction.
#      */
# 	public final Cell<A> hold(final A initValue) {
    def hold(self, init_value: A) -> "Cell[A]":
        """
        Create a `Cell` with the specified initial value, that is
        updated by this stream's event values.

        There is an implicit delay: State updates caused by event firings
        don't become visible as the cell's current value as viewed by
        `Stream.snapshot(Cell, Callable)` until the following transaction.
        To put this another way, `Stream.snapshot(Cell, Callable)` always
        sees the value of a cell as it was before any state changes from
        the current transaction.
        """
        assert isinstance(self, StreamWithSend)
        return Transaction._apply(
            lambda trans: Cell(self, init_value))
# 		return Transaction.apply(new Lambda1<Transaction, Cell<A>>() {
# 			public Cell<A> apply(Transaction trans) {
# 			    return new Cell<A>(Stream.this, initValue);
# 			}
# 		});
# 	}
# 
# 	/**
# 	 * A variant of {@link #hold(Object)} with an initial value captured by {@link Cell#sampleLazy()}.
# 	 */
# 	public final Cell<A> holdLazy(final Lazy<A> initValue) {
    def hold_lazy(self, init_value: Lazy[A]) -> "Cell[A]":
        """
        A variant of {@link #hold(Object)} with an initial value captured
        by `Cell.sample_lazy`.
        """
        return Transaction._apply(
            lambda trans: self._hold_lazy(trans, init_value))
# 		return Transaction.apply(new Lambda1<Transaction, Cell<A>>() {
# 			public Cell<A> apply(Transaction trans) {
# 			    return holdLazy(trans, initValue);
# 			}
# 		});
# 	}
# 
# 	final Cell<A> holdLazy(Transaction trans, final Lazy<A> initValue) {
    def _hold_lazy(self, _: Transaction, init_value: Lazy[A]) -> "Cell[A]":
        return LazyCell(self, init_value)
# 	    return new LazyCell<A>(this, initValue);
# 	}
# 
# 	/**
# 	 * Variant of {@link #snapshot(Cell, Lambda2)} that captures the cell's value
# 	 * at the time of the event firing, ignoring the stream's value.
# 	 */
# 	public final <B> Stream<B> snapshot(Cell<B> c)
# 	{
# 	    return snapshot(c, new Lambda2<A,B,B>() {
# 	    	public B apply(A a, B b) {
# 	    		return b;
# 	    	}
# 	    });
# 	}
# 
# 	/**
# 	 * Return a stream whose events are the result of the combination using the specified
# 	 * function of the input stream's event value and the value of the cell at that time.
#      * <P>
#      * There is an implicit delay: State updates caused by event firings being held with
#      * {@link Stream#hold(Object)} don't become visible as the cell's current value until
#      * the following transaction. To put this another way, {@link Stream#snapshot(Cell, Lambda2)}
#      * always sees the value of a cell as it was before any state changes from the current
#      * transaction.
#      */
# 	public final <B,C> Stream<C> snapshot(final Cell<B> c, final Lambda2<A,B,C> f)
    def snapshot(self,
            c: "Cell[B]",
            f: Callable[[A,B],C] = (lambda a, b: b)) -> "Stream[C]":
        """
        Return a stream whose events are the result of the combination using
        the specified function of the input stream's event value and
        the value of the cell at that time.

        If the function is omitted, return a stream that captures the cell's
        value at the time of the event firing, ignoring the stream's value.

        There is an implicit delay: State updates caused by event firings
        being held with `Stream.hold` don't become visible as the cell's
        current value until the following transaction. To put this another
        way, `Stream.snapshot` always sees the value of a cell as it was
        before any state changes from the current transaction.
        """
        out: StreamWithSend[C] = StreamWithSend()
        l = self._listen(out._node,
            lambda trans2, a: out._send(trans2, f(a, c._sample_no_trans())))
        return out._unsafe_add_cleanup(l)
# 	{
# 	    final Stream<A> ev = this;
# 		final StreamWithSend<C> out = new StreamWithSend<C>();
#         Listener l = listen_(out.node, new TransactionHandler<A>() {
#         	public void run(Transaction trans2, A a) {
# 	            out.send(trans2, f.apply(a, c.sampleNoTrans()));
# 	        }
#         });
#         return out.unsafeAddCleanup(l);
# 	}
# 
# 	/**
# 	 * Variant of {@link #snapshot(Cell, Lambda2)} that captures the values of
# 	 * two cells.
#      */
# 	public final <B,C,D> Stream<D> snapshot(final Cell<B> cb, final Cell<C> cc, final Lambda3<A,B,C,D> fn)
# 	{
# 		return this.snapshot(cb, new Lambda2<A,B,D>() {
# 	    	public D apply(A a, B b) {
# 	    		return fn.apply(a, b, cc.sample());
# 	    	}
# 		});
# 	}
# 
# 	/**
# 	 * Variant of {@link #snapshot(Cell, Lambda2)} that captures the values of
# 	 * three cells.
#      */
# 	public final <B,C,D,E> Stream<E> snapshot(final Cell<B> cb, final Cell<C> cc, final Cell<D> cd, final Lambda4<A,B,C,D,E> fn)
# 	{
# 		return this.snapshot(cb, new Lambda2<A,B,E>() {
# 	    	public E apply(A a, B b) {
# 	    		return fn.apply(a, b, cc.sample(), cd.sample());
# 	    	}
# 		});
# 	}
# 
# 	/**
# 	 * Variant of {@link #snapshot(Cell, Lambda2)} that captures the values of
# 	 * four cells.
#      */
# 	public final <B,C,D,E,F> Stream<F> snapshot(final Cell<B> cb, final Cell<C> cc, final Cell<D> cd, final Cell<E> ce, final Lambda5<A,B,C,D,E,F> fn)
# 	{
# 		return this.snapshot(cb, new Lambda2<A,B,F>() {
# 	    	public F apply(A a, B b) {
# 	    		return fn.apply(a, b, cc.sample(), cd.sample(), ce.sample());
# 	    	}
# 		});
# 	}
# 
# 	/**
# 	 * Variant of {@link #snapshot(Cell, Lambda2)} that captures the values of
# 	 * five cells.
#      */
# 	public final <B,C,D,E,F,G> Stream<G> snapshot(final Cell<B> cb, final Cell<C> cc, final Cell<D> cd, final Cell<E> ce, final Cell<F> cf, final Lambda6<A,B,C,D,E,F,G> fn)
# 	{
# 		return this.snapshot(cb, new Lambda2<A,B,G>() {
# 	    	public G apply(A a, B b) {
# 	    		return fn.apply(a, b, cc.sample(), cd.sample(), ce.sample(), cf.sample());
# 	    	}
# 		});
# 	}
# 
#     /**
#      * Variant of {@link Stream#merge(Stream, Lambda2)} that merges two streams and will drop an event
#      * in the simultaneous case.
#      * <p>
#      * In the case where two events are simultaneous (i.e. both
#      * within the same transaction), the event from <em>this</em> will take precedence, and
#      * the event from <em>s</em> will be dropped.
#      * If you want to specify your own combining function, use {@link Stream#merge(Stream, Lambda2)}.
#      * s1.orElse(s2) is equivalent to s1.merge(s2, (l, r) -&gt; l).
#      * <p>
#      * The name orElse() is used instead of merge() to make it really clear that care should
#      * be taken, because events can be dropped.
#      */
# 	public final Stream<A> orElse(final Stream<A> s)
    def or_else(self, s: "Stream[A]") -> "Stream[A]":
        """
        Variant of `Stream.merge(Stream, Callable)` that merges two streams
        and will drop an event in the simultaneous case.

        In the case where two events are simultaneous (i.e. both within
        the same transaction), the event from **this** will take precedence,
        and the event from *s* will be dropped. If you want to specify your
        own combining function, use `Stream.merge(Stream, Callable)`.
        s1.or_else(s2) is equivalent to s1.merge(s2, lambda l, r: l).

        The name or_else() is used instead of merge() to make it really
        clear that care should be taken, because events can be dropped.
        """
        return self.merge(s, lambda left, right: left)
# 	{
# 	    return merge(s, new Lambda2<A,A,A>() {
#             public A apply(A left, A right) { return left; }
#         });
# 	}
# 
# 	private static <A> Stream<A> merge(final Stream<A> ea, final Stream<A> eb)
    @staticmethod
    def _merge(ea: "Stream[A]", eb: "Stream[A]") -> "Stream[A]":
        out: StreamWithSend[A] = StreamWithSend()
        left = Node(0)
        right = out._node
        node_target_: List[Target] = [None]
        left._link_to(lambda _: None, right, node_target_)
        node_target = node_target_[0]
        h = out._send
        l1 = ea._listen(left, h)
        l2 = eb._listen(right, h)
        def unlisten(_self: Listener) -> None:
            left._unlink_to(node_target)
        return out \
            ._unsafe_add_cleanup(l1) \
            ._unsafe_add_cleanup(l2) \
            ._unsafe_add_cleanup(Listener(unlisten))
# 	{
# 	    final StreamWithSend<A> out = new StreamWithSend<A>();
#         final Node left = new Node(0);
#         final Node right = out.node;
#         Node.Target[] node_target_ = new Node.Target[1];
#         left.linkTo(null, right, node_target_);
#         final Node.Target node_target = node_target_[0];
#         TransactionHandler<A> h = new TransactionHandler<A>() {
#         	public void run(Transaction trans, A a) {
# 	            out.send(trans, a);
# 	        }
#         };
#         Listener l1 = ea.listen_(left, h);
#         Listener l2 = eb.listen_(right, h);
#         return out.unsafeAddCleanup(l1).unsafeAddCleanup(l2).unsafeAddCleanup(new Listener() {
#             public void unlisten() {
#                 left.unlinkTo(node_target);
#             }
#         });
# 	}
# 
#     /**
#      * Merge two streams of the same type into one, so that events on either input appear
#      * on the returned stream.
#      * <p>
#      * If the events are simultaneous (that is, one event from this and one from <em>s</em>
#      * occurring in the same transaction), combine them into one using the specified combining function
#      * so that the returned stream is guaranteed only ever to have one event per transaction.
#      * The event from <em>this</em> will appear at the left input of the combining function, and
#      * the event from <em>s</em> will appear at the right.
#      * @param f Function to combine the values. It may construct FRP logic or use
#      *    {@link Cell#sample()}. Apart from this the function must be <em>referentially transparent</em>.
#      */
#     public final Stream<A> merge(final Stream<A> s, final Lambda2<A,A,A> f)
    def merge(self, s: "Stream[A]", f: Callable[[A, A], A]) -> "Stream[A]":
        """
        Merge two streams of the same type into one, so that events on
        either input appear on the returned stream.

        If the events are simultaneous (that is, one event from this and one
        from **s** occurring in the same transaction), combine them into one
        using the specified combining function so that the returned stream
        is guaranteed only ever to have one event per transaction. The event
        from **self** will appear at the left input of the combining
        function, and the event from **s** will appear at the right.
        @param f Function to combine the values. It may construct FRP logic
            or use `Cell.sample()`. Apart from this the function must be
            **referentially transparent**.
        """
        return Transaction._apply(
            lambda trans: Stream._merge(self, s)._coalesce(trans, f))
#     {
# 	    return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
# 	    	public Stream<A> apply(Transaction trans) {
#                 return Stream.<A>merge(Stream.this, s).coalesce(trans, f);
# 	    	}
# 	    });
#     }
# 
#     /**
#      * Variant of {@link #orElse(Stream)} that merges a collection of streams.
#      */
#     public static <A> Stream<A> orElse(Iterable<Stream<A>> ss) {
    @staticmethod
    def or_else_(*streams: "Stream[A]") -> "Stream[A]":
        """
        Variant of `or_else(Stream)` that merges a collection of streams.
        """
        return Stream.merge_(lambda left, right: left, *streams)

#         return Stream.<A>merge(ss, new Lambda2<A,A,A>() {
#             public A apply(A left, A right) { return left; }
#         });
#     }
# 
#     /**
#      * Variant of {@link #merge(Stream,Lambda2)} that merges a collection of streams.
#      */
#     public static <A> Stream<A> merge(Iterable<Stream<A>> ss, final Lambda2<A,A,A> f) {
    @staticmethod
    def merge_(
            f: Callable[[A,A],A],
            *streams: "Stream[A]") -> "Stream[A]":
        """
        Variant of `merge(Stream, Callable)` that merges a collection of streams.
        """
        return Stream._merge_many(streams, 0, len(streams), f)
#         Vector<Stream<A>> v = new Vector<Stream<A>>();
#         for (Stream<A> s : ss)
#             v.add(s);
#         return merge(v, 0, v.size(), f);
#     }
# 
#     private static <A> Stream<A> merge(Vector<Stream<A>> sas, int start, int end, final Lambda2<A,A,A> f) {
    @staticmethod
    def _merge_many(
            streams: Sequence["Stream[A]"],
            start: int,
            end: int,
            f: Callable[[A,A],A]) -> "Stream[A]":
        length = end - start
        if length == 0:
            return Stream.never()
        elif length == 1:
            return streams[start]
        elif length == 2:
            left = streams[start]
            right = streams[start + 1]
            return left.merge(right, f)
        else:
            mid = (start + end) // 2
            left = Stream._merge_many(streams, start, mid, f)
            right = Stream._merge_many(streams, mid, end, f)
            return left.merge(right, f)
#         int len = end - start;
#         if (len == 0) return new Stream<A>(); else
#         if (len == 1) return sas.get(start); else
#         if (len == 2) return sas.get(start).merge(sas.get(start+1), f); else {
#             int mid = (start + end) / 2;
#             return Stream.<A>merge(sas, start, mid, f).merge(Stream.<A>merge(sas, mid, end, f), f);
#         }
#     }
# 
# 	private final Stream<A> coalesce(Transaction trans1, final Lambda2<A,A,A> f)
    def _coalesce(self,
            trans1: Transaction,
            f: Callable[[A,A],A]) -> "Stream[A]":
        out: StreamWithSend[A]  = StreamWithSend()
        h = CoalesceHandler(f, out)
        l = self._listen_internal(out._node, trans1, h, False)
        return out._unsafe_add_cleanup(l)
# 	{
# 	    final Stream<A> ev = this;
# 	    final StreamWithSend<A> out = new StreamWithSend<A>();
#         TransactionHandler<A> h = new CoalesceHandler<A>(f, out);
#         Listener l = listen(out.node, trans1, h, false);
#         return out.unsafeAddCleanup(l);
#     }
# 
#     /**
#      * Clean up the output by discarding any firing other than the last one.
#      */
#     final Stream<A> lastFiringOnly(Transaction trans)
    def _last_firing_only(self, trans: Transaction) -> "Stream[A]":
        """
        Clean up the output by discarding any firing other than the last one.
        """
        return self._coalesce(trans, lambda first, second: second)
#     {
#         return coalesce(trans, new Lambda2<A,A,A>() {
#         	public A apply(A first, A second) { return second; }
#         });
#     }
# 
#     /**
#      * Return a stream that only outputs events for which the predicate returns true.
#      */
#     public final Stream<A> filter(final Lambda1<A,Boolean> predicate)
    def filter(self, predicate: Callable[[A], bool]) -> "Stream[A]":
        """
        Return a stream that only outputs events for which the predicate
        returns true.
        """
        out: StreamWithSend[A] = StreamWithSend()
        def handler(trans2: Transaction, a: A) -> None:
            if predicate(a):
                out._send(trans2, a)
        l = self._listen(out._node, handler)
        return out._unsafe_add_cleanup(l)
#     {
#         final Stream<A> ev = this;
#         final StreamWithSend<A> out = new StreamWithSend<A>();
#         Listener l = listen_(out.node, new TransactionHandler<A>() {
#         	public void run(Transaction trans2, A a) {
# 	            if (predicate.apply(a)) out.send(trans2, a);
# 	        }
#         });
#         return out.unsafeAddCleanup(l);
#     }
# 
#     /**
#      * Return a stream that only outputs events that have present
#      * values, removing the {@link java.util.Optional} wrapper, discarding empty values.
#      */
#     public static <A> Stream<A> filterOptional(final Stream<Optional<A>> ev)
#     {
#         final StreamWithSend<A> out = new StreamWithSend<A>();
#         Listener l = ev.listen_(out.node, new TransactionHandler<Optional<A>>() {
#         	public void run(Transaction trans2, Optional<A> oa) {
# 	            if (oa.isPresent()) out.send(trans2, oa.get());
# 	        }
#         });
#         return out.unsafeAddCleanup(l);
#     }
# 
#     /**
#      * Return a stream that only outputs events from the input stream
#      * when the specified cell's value is true.
#      */
#     public final Stream<A> gate(Cell<Boolean> c)
    def gate(self, c: "Cell[bool]") -> "Stream[A]":
        """
        Return a stream that only outputs events from the input stream
        when the specified cell's value is `True`.
        """
        return self \
            .snapshot(c, lambda a, pred: a if pred else None) \
            .filter(lambda x: x is not None)
#     {
#         return Stream.filterOptional(
#             snapshot(c, new Lambda2<A,Boolean,Optional<A>>() {
#                 public Optional<A> apply(A a, Boolean pred) { return pred ? Optional.of(a) : Optional.<A>empty(); }
#             })
#         );
#     }
# 
#     /**
#      * Transform an event with a generalized state loop (a Mealy machine). The function
#      * is passed the input and the old state and returns the new state and output value.
#      * @param f Function to apply to update the state. It may construct FRP logic or use
#      *    {@link Cell#sample()} in which case it is equivalent to {@link Stream#snapshot(Cell)}ing the
#      *    cell. Apart from this the function must be <em>referentially transparent</em>.
#      */
#     public final <B,S> Stream<B> collect(final S initState, final Lambda2<A, S, Tuple2<B, S>> f)
    def collect(self,
            init_state: S,
            f: Callable[[A, S], Tuple[B, S]]) -> "Stream[B]":
        """
        Transform an event with a generalized state loop (a Mealy machine).
        The function is passed the input and the old state and returns
        the new state and output value.

        @param f Function to apply to update the state. It may construct FRP
            logic or use `Cell.sample()` in which case it is equivalent
            to `Stream.snapshot()`ing the cell. Apart from this the function
            must be **referentially transparent**.
        """
        return self.collect_lazy(Lazy(lambda: init_state), f)
#     {
#         return collectLazy(new Lazy<S>(initState), f);
#     }
# 
#     /**
#      * A variant of {@link #collect(Object, Lambda2)} that takes an initial state returned by
#      * {@link Cell#sampleLazy()}.
#      */
#     public final <B,S> Stream<B> collectLazy(final Lazy<S> initState, final Lambda2<A, S, Tuple2<B, S>> f)
    def collect_lazy(self,
            init_state: Lazy[S],
            f: Callable[[A, S], Tuple[B, S]]) -> "Stream[B]":
        """
        A variant of `collect()` that takes an initial state returned by
        `Cell.sample_lazy()`.
        """
#     {
#         return Transaction.<Stream<B>>run(new Lambda0<Stream<B>>() {
#             public Stream<B> apply() {
        def handler() -> Stream[B]:
            ea = self
            es: StreamLoop[S] = StreamLoop()
            s = es.hold_lazy(init_state)
            ebs = ea.snapshot(s, f)
            eb = ebs.map(lambda bs: bs[0])
            es_out = ebs.map(lambda bs: bs[1])
            es.loop(es_out)
            return eb
#                 final Stream<A> ea = Stream.this;
#                 StreamLoop<S> es = new StreamLoop<S>();
#                 Cell<S> s = es.holdLazy(initState);
#                 Stream<Tuple2<B,S>> ebs = ea.snapshot(s, f);
#                 Stream<B> eb = ebs.map(new Lambda1<Tuple2<B,S>,B>() {
#                     public B apply(Tuple2<B,S> bs) { return bs.a; }
#                 });
#                 Stream<S> es_out = ebs.map(new Lambda1<Tuple2<B,S>,S>() {
#                     public S apply(Tuple2<B,S> bs) { return bs.b; }
#                 });
#                 es.loop(es_out);
#                 return eb;
#             }
#         });
        return Transaction.run(handler)
#     }
# 
#     /**
#      * Accumulate on input event, outputting the new state each time.
#      * @param f Function to apply to update the state. It may construct FRP logic or use
#      *    {@link Cell#sample()} in which case it is equivalent to {@link Stream#snapshot(Cell)}ing the
#      *    cell. Apart from this the function must be <em>referentially transparent</em>.
#      */
#     public final <S> Cell<S> accum(final S initState, final Lambda2<A, S, S> f)
    def accum(self, init_state: S, f: Callable[[A, S], S]) -> "Cell[S]":
        """
        Accumulate on input event, outputting the new state each time.

        @param f Function to apply to update the state. It may construct FRP
            logic or use `Cell.sample()` in which case it is equivalent
            to `Stream.snapshot()`ing the cell. Apart from this the function
            must be **referentially transparent**.
        """
        return self.accum_lazy(Lazy(lambda: init_state), f)
#     {
#         return accumLazy(new Lazy<S>(initState), f);
#     }
# 
#     /**
#      * A variant of {@link #accum(Object, Lambda2)} that takes an initial state returned by
#      * {@link Cell#sampleLazy()}.
#      */
#     public final <S> Cell<S> accumLazy(final Lazy<S> initState, final Lambda2<A, S, S> f)
    def accum_lazy(self,
            init_state: Lazy[S],
            f: Callable[[A, S], S]) -> "Cell[S]":
        """
        A variant of `accum()` that takes an initial state returned by
        `Cell.sample_lazy()`.
        """
#     {
#         return Transaction.<Cell<S>>run(new Lambda0<Cell<S>>() {
#             public Cell<S> apply() {
        def handler() -> Cell[S]:
            ea = self
            es: StreamLoop[S] = StreamLoop()
            s = es.hold_lazy(init_state)
            es_out = ea.snapshot(s, f)
            es.loop(es_out)
            return es_out.hold_lazy(init_state)
#                 final Stream<A> ea = Stream.this;
#                 StreamLoop<S> es = new StreamLoop<S>();
#                 Cell<S> s = es.holdLazy(initState);
#                 Stream<S> es_out = ea.snapshot(s, f);
#                 es.loop(es_out);
#                 return es_out.holdLazy(initState);
#             }
#         });
        return Transaction.run(handler)
#     }
# 
#     /**
#      * Return a stream that outputs only one value: the next event of the
#      * input stream, starting from the transaction in which once() was invoked.
#      */
#     public final Stream<A> once()
#     {
#         // This is a bit long-winded but it's efficient because it deregisters
#         // the listener.
#         final Stream<A> ev = this;
#         final Listener[] la = new Listener[1];
#         final StreamWithSend<A> out = new StreamWithSend<A>();
#         la[0] = ev.listen_(out.node, new TransactionHandler<A>() {
#         	public void run(Transaction trans, A a) {
# 	            if (la[0] != null) {
#                     out.send(trans, a);
# 	                la[0].unlisten();
# 	                la[0] = null;
# 	            }
# 	        }
#         });
#         return out.unsafeAddCleanup(la[0]);
#     }
# 
#     /**
#      * This is not thread-safe, so one of these two conditions must apply:
#      * 1. We are within a transaction, since in the current implementation
#      *    a transaction locks out all other threads.
#      * 2. The object on which this is being called was created has not yet
#      *    been returned from the method where it was created, so it can't
#      *    be shared between threads.
#      */
#     Stream<A> unsafeAddCleanup(Listener cleanup)
    def _unsafe_add_cleanup(self, cleanup: Listener) -> "Stream[A]":
        """
        This is not thread-safe, so one of these two conditions must apply:
        1. We are within a transaction, since in the current implementation
           a transaction locks out all other threads.
        2. The object on which this is being called was created has not yet
           been returned from the method where it was created, so it can't
           be shared between threads.
        """
        self._finalizers.append(cleanup)
        return self
#     {
#         finalizers.add(cleanup);
#         return this;
#     }
# 
#     /**
#      * Attach a listener to this stream so that its {@link Listener#unlisten()} is invoked
#      * when this stream is garbage collected. Useful for functions that initiate I/O,
#      * returning the result of it through a stream.
#      * <P>
#      * You must use this only with listeners returned by {@link #listenWeak(Handler)} so that
#      * things don't get kept alive when they shouldn't.
#      */
#     public Stream<A> addCleanup(final Listener cleanup) {
#         return Transaction.run(new Lambda0<Stream<A>>() {
#             public Stream<A> apply() {
#                 List<Listener> fsNew = new ArrayList<Listener>(finalizers);
#                 fsNew.add(cleanup);
#                 return new Stream<A>(node, fsNew, firings);
#             }
#         });
#     }
# 
# 	@Override
# 	protected void finalize() throws Throwable {
# 		for (Listener l : finalizers)
# 			l.unlisten();
# 	}
# }


# class StreamWithSend<A> extends Stream<A> {
class StreamWithSend(Stream[A]):
#
    def __init__(self) -> None:
        super().__init__(Node(0), [], [])

# 	protected void send(Transaction trans, final A a) {
    def _send(self, trans: Transaction, a: A) -> None:
        if len(self._firings) == 0:
            trans.last(lambda: self._firings.clear())
        self._firings.append(a)
# 		if (firings.isEmpty())
# 			trans.last(new Runnable() {
# 				public void run() {
# 					firings.clear();
# 				}
# 			});
# 		firings.add(a);
# 
        with Transaction._listeners_lock:
            listeners = set(self._node._listeners)
# 		HashSet<Node.Target> listeners;
#         synchronized (Transaction.listenersLock) {
#             listeners = new HashSet<Node.Target>(node.listeners);
#         }
        for target in listeners:
            def handler(
                    trans2: Transaction,
                    # Pass target as default argument, as loop variables
                    # shouldn't be captured by a closure
                    target: Target = target) -> None:
                Transaction.in_callback += 1
                try:
                    # Don't allow transactions to interfere with Sodium
                    # internals.
                    uta = target.action() # Dereference the weak reference
                    if uta is not None: # If it hasn't been GC'ed, call it
                        uta(trans2, a)
                except:
                    traceback.print_exc()
                finally:
                    Transaction.in_callback -= 1
            trans._prioritized(target.node, handler)
# 		for (final Node.Target target : listeners) {
#             trans.prioritized(target.node, new Handler<Transaction>() {
#                 public void run(Transaction trans2) {
#                     Transaction.inCallback++;
#                     try {  // Don't allow transactions to interfere with Sodium
#                            // internals.
#                         // Dereference the weak reference
#                         TransactionHandler<Unit> uta = target.action.get();
#                         if (uta != null)  // If it hasn't been gc'ed..., call it
#                             ((TransactionHandler<A>)uta).run(trans2, a);
#                     } catch (Throwable t) {
#                         t.printStackTrace();
#                     }
#                     finally {
#                         Transaction.inCallback--;
#                     }
#                 }
#             });
# 		}
# 	}
# 
# }


# package nz.sodium;
# 
# /**
#  * A stream that allows values to be pushed into it, acting as an interface between the
#  * world of I/O and the world of FRP. Code that exports StreamSinks for read-only use
#  * should downcast to {@link Stream}.
#  */
class StreamSink(StreamWithSend[A]):
    """
    A stream that allows values to be pushed into it, acting as an interface
    between the world of I/O and the world of FRP. Code that exports
    StreamSinks for read-only use should downcast to `Stream`.
    """
# public class StreamSink<A> extends StreamWithSend<A> {
#     /**
#      * Construct a StreamSink that allows send() to be called once on it per transaction.
#      * If you call send() more than once, it will throw an exception. If you need to do
#      * this, then use {@link #StreamSink(Lambda2)}.
#      */
    def __init__(self, f: Callable[[A, A], A] = None) -> None:
        """
        Construct a StreamSink. Use the provided function to combine values,
        that were sent to the stream during the same transaction, into a
        single event. If the function is `None`, send() throws an exception,
        if it is called more then once per transaction.

        The combining function should be **associative**.

        @param f Function to combine the values. It may construct
            FRP logic or use `Cell.sample()`. Apart from this the function
            must be **referentially transparent**.
        """
        super().__init__()
        def error(left: A, right: A) -> A:
            raise RuntimeError("send() called more than once per "
                "transaction, which isn't allowed. Did you want to combine "
                "the events? Then pass a combining function to your "
                "StreamSink constructor.");
        self._coalescer = CoalesceHandler(
            f if f is not None else error, self)
#     public StreamSink() {
#         this(new Lambda2<A,A,A>() {
#              public A apply(A left, A right) {
#                  throw new RuntimeException("send() called more than once per transaction, which isn't allowed. Did you want to combine the events? Then pass a combining function to your StreamSink constructor.");
#              }
#          });
#     }
#     /**
#      * If you send more than one event in a transaction, they are combined into a
#      * single event using the specified function. The combining function should be
#      * <em>associative</em>.
#      * @param f Function to combine the values. It may construct FRP logic or use
#      *    {@link Cell#sample()}. Apart from this the function must be <em>referentially transparent</em>.
#      */
#     public StreamSink(Lambda2<A,A,A> f) {
#         this.coalescer = new CoalesceHandler<A>(f, this);
#     }
# 
#     private CoalesceHandler<A> coalescer;
# 
#     /**
#      * Send a value to be made available to consumers of the stream. send(A) may not be used inside
#      * handlers registered with {@link Stream#listen(Handler)} or {@link Cell#listen(Handler)}.
#      * An exception will be thrown, because StreamSink is for interfacing I/O to FRP only.
#      * You are not meant to use this to define your own primitives.
#      * @param a Value to push into the cell.
#      */
    def send(self, a: A) -> None:
        """
        Send a value to be made available to consumers of the stream.
        send(A) may not be used inside handlers registered with
        `Stream.listen(Handler)` or `Cell.listen(Handler)`. An exception
        will be thrown, because StreamSink is for interfacing I/O to FRP only.
        You are not meant to use this to define your own primitives.
        @param a Value to push into the cell.
        """
        def handler(trans: Transaction) -> None:
            if trans.in_callback > 0:
                raise RuntimeError("You are not allowed to use send() "
                    "inside a Sodium callback");
            self._coalescer(trans, a)
        Transaction._run_handler(handler)
# 	public void send(final A a) {
# 		Transaction.run(new Handler<Transaction>() {
# 			public void run(Transaction trans) {
#                 if (trans.inCallback > 0)
#                     throw new RuntimeException("You are not allowed to use send() inside a Sodium callback");
#                 coalescer.run(trans, a);
#             }
# 		});
# 	}
# }


# /**
#  * A forward reference for a {@link Stream} equivalent to the Stream that is referenced.
#  */
# public class StreamLoop<A> extends StreamWithSend<A> {
class StreamLoop(StreamWithSend[A]):
    """
    A forward reference for a `Stream` equivalent to the Stream that
    is referenced.
    """
#     boolean assigned = false;
# 
#     public StreamLoop()
    def __init__(self) -> None:
        super().__init__()
        self._assigned = False
        if Transaction.get_current_transaction() is None:
            raise RuntimeError("StreamLoop/CellLoop must be used within "
                "an explicit transaction")
#     {
#     	if (Transaction.getCurrentTransaction() == null)
#     	    throw new RuntimeException("StreamLoop/CellLoop must be used within an explicit transaction");
#     }
# 
#     /**
#      * Resolve the loop to specify what the StreamLoop was a forward reference to. It
#      * must be invoked inside the same transaction as the place where the StreamLoop is used.
#      * This requires you to create an explicit transaction with {@link Transaction#run(Lambda0)}
#      * or {@link Transaction#runVoid(Runnable)}.
#      */
#     public void loop(final Stream<A> ea_out)
    def loop(self, ea_out: Stream[A]) -> None:
        """
        Resolve the loop to specify what the StreamLoop was a forward
        reference to. It must be invoked inside the same transaction as
        the place where the StreamLoop is used.  This requires you to create
        an explicit transaction with `Transaction.run()`.
        """
        if self._assigned:
            raise RuntimeError("StreamLoop looped more than once")
        self._assigned = True
        Transaction.run(
            lambda: self._unsafe_add_cleanup(
                ea_out._listen(self._node, self._send)
            )
        )
#     {
#         if (assigned)
#             throw new RuntimeException("StreamLoop looped more than once");
#         assigned = true;
#         final StreamLoop<A> me = this;
#         Transaction.runVoid(new Runnable() {
#             public void run() {
#                 unsafeAddCleanup(ea_out.listen_(StreamLoop.this.node, new TransactionHandler<A>() {
#                     public void run(Transaction trans, A a) {
#                         me.send(trans, a);
#                     }
#                 }));
#             }
#         });
#     }
# }


class CoalesceHandler(Generic[A]):

    def __init__(self, f: Callable[[A,A], A], out: StreamWithSend[A]):
        self._f = f
        self._out = out
        self._accum_valid = False
        self._accum: A = None


    def __call__(self, trans1: Transaction, a: A) -> None:
        if self._accum_valid:
            self._accum = self._f(self._accum, a)
        else:
            def handler(trans2: Optional[Transaction]) -> None:
                self._out._send(trans2, self._accum)
                self._accum_valid = False
                self._accum = None
            trans1._prioritized(self._out._node, handler)
            self._accum = a
            self._accum_valid = True


# /**
#  * Represents a value of type A that changes over time.
#  */
# public class Cell<A> {
class Cell(Generic[A]):
    """ Represents a value of type A that changes over time. """
# 	final Stream<A> str;
# 	A value;
# 	A valueUpdate;
# 	private Listener cleanup;
#     Lazy<A> lazyInitValue;  // Used by LazyCell
# 
# 	/**
# 	 * A cell with a constant value.
# 	 */
#     public Cell(A value)
    @staticmethod
    def constant(value: A) -> "Cell[A]":
        return Cell(Stream.never(), value)
#     {
#     	this.str = new Stream<A>();
#     	this.value = value;
#     }
# 
#     Cell(final Stream<A> str, A initValue)
    def __init__(self, stream: StreamWithSend[A], init_value: A) -> None:
        self._stream = stream
        self._value = init_value
        self._value_update: A = None
        self._cleanup: Listener = None
        self._lazy_init_value: Lazy[A] = None # Use by LazyCell
        def handler(trans1: Transaction) -> None:
            def handler2(trans2: Transaction, a: A) -> None:
                if self._value_update is None:
                    def run() -> None:
                        self._value = self._value_update
                        self._lazy_init_value = None
                        self._value_update = None
                    trans2.last(run)
                self._value_update = a
            self._cleanup = stream._listen_internal(
                NODE_NULL, trans1, handler2, False)
        Transaction._run_handler(handler)
#     {
#     	this.str = str;
#     	this.value = initValue;
#     	Transaction.run(new Handler<Transaction>() {
#     		public void run(Transaction trans1) {
# 	    		Cell.this.cleanup = str.listen(Node.NULL, trans1, new TransactionHandler<A>() {
# 	    			public void run(Transaction trans2, A a) {
# 			    		if (Cell.this.valueUpdate == null) {
# 			    			trans2.last(new Runnable() {
# 			    				public void run() {
# 				    				Cell.this.value = Cell.this.valueUpdate;
# 				    				Cell.this.lazyInitValue = null;
# 				    				Cell.this.valueUpdate = null;
# 				    			}
# 			    			});
# 			    		}
# 			    		Cell.this.valueUpdate = a;
# 			    	}
# 	    		}, false);
#     		}
#     	});
#     }
# 
#     /**
#      * @return The value including any updates that have happened in this transaction.
#      */
#     final A newValue()
#     {
#     	return valueUpdate == null ? sampleNoTrans() :  valueUpdate;
#     }
# 
#     /**
#      * Sample the cell's current value.
#      * <p>
#      * It may be used inside the functions passed to primitives that apply them to {@link Stream}s,
#      * including {@link Stream#map(Lambda1)} in which case it is equivalent to snapshotting the cell,
#      * {@link Stream#snapshot(Cell, Lambda2)}, {@link Stream#filter(Lambda1)} and
#      * {@link Stream#merge(Stream, Lambda2)}.
#      * It should generally be avoided in favour of {@link #listen(Handler)} so you don't
#      * miss any updates, but in many circumstances it makes sense.
#      */
#     public final A sample()
    def sample(self) -> A:
        """
        Sample the cell's current value.

        It may be used inside the functions passed to primitives that apply
        them to `Stream`s, including `Stream.map` in which case it is
        equivalent to snapshotting the cell, `Stream.snapshot`,
        `Stream.filter` and `Stream.merge`.

        It should generally be avoided in favour of `listen` so you don't
        miss any updates, but in many circumstances it makes sense.
        """
        return Transaction._apply(lambda trans: self._sample_no_trans())
#     {
#         return Transaction.apply(new Lambda1<Transaction, A>() {
#         	public A apply(Transaction trans) {
#         		return sampleNoTrans();
#         	}
#         });
#     }
# 
#     private static class LazySample<A> {
#         LazySample(Cell<A> cell) {
#             this.cell = cell;
#         }
#         Cell<A> cell;
#         boolean hasValue;
#         A value;
#     }
# 
#     /**
#      * A variant of {@link #sample()} that works with {@link CellLoop}s when they haven't been looped yet.
#      * It should be used in any code that's general enough that it could be passed a {@link CellLoop}.
#      * @see Stream#holdLazy(Lazy) Stream.holdLazy()
#      */
#     public final Lazy<A> sampleLazy() {
    def sample_lazy(self) -> Lazy[A]:
        """
        A variant of `sample()` that works with `CellLoop`s when they
        haven't been looped yet.  It should be used in any code that's
        general enough that it could be passed a `CellLoop`.
        """
        return Transaction._apply(lambda trans: self._sample_lazy(trans))
#         final Cell<A> me = this;
#         return Transaction.apply(new Lambda1<Transaction, Lazy<A>>() {
#         	public Lazy<A> apply(Transaction trans) {
#         	    return me.sampleLazy(trans);
#             }
#         });
#     }
# 
#     final Lazy<A> sampleLazy(Transaction trans) {
    def _sample_lazy(self, trans: Transaction) -> Lazy[A]:
        @dataclass
        class LazySample:
            cell: Cell[A]
            has_value: bool
            value: A
        s = LazySample(cell=self, has_value=None, value=None)
        def handler() -> None:
            s.value = self._value_update \
                if self._value_update is not None \
                else self._sample_no_trans()
            s.has_value = True
            s.cell = None
        trans.last(handler)
        return Lazy(lambda: s.value if s.has_value else s.cell.sample())
#         final Cell<A> me = this;
#         final LazySample<A> s = new LazySample<A>(me);
#         trans.last(new Runnable() {
#             public void run() {
#                 s.value = me.valueUpdate != null ? me.valueUpdate : me.sampleNoTrans();
#                 s.hasValue = true;
#                 s.cell = null;
#             }
#         });
#         return new Lazy<A>(new Lambda0<A>() {
#             public A apply() {
#                 if (s.hasValue)
#                     return s.value;
#                 else
#                     return s.cell.sample();
#             }
#         });
#     }
# 
#     A sampleNoTrans()
    def _sample_no_trans(self) -> A:
        return self._value
#     {
#         return value;
#     }
# 
#     final Stream<A> updates()
    def _updates(self) -> Stream[A]:
        return self._stream
#     {
#         return str;
#     }
# 
#     final Stream<A> value(Transaction trans1)
    def _value_stream(self, trans1: Transaction) -> Stream[A]:
        s_spark: StreamWithSend[Unit] = StreamWithSend()
        trans1._prioritized(
            s_spark._node, lambda trans2: s_spark._send(trans2, UNIT))
        s_initial: Stream[A] = s_spark.snapshot(self)
        return s_initial.merge(self._updates(), lambda left, right: right)
#     {
#     	final StreamWithSend<Unit> sSpark = new StreamWithSend<Unit>();
#         trans1.prioritized(sSpark.node, new Handler<Transaction>() {
#             public void run(Transaction trans2) {
#                 sSpark.send(trans2, Unit.UNIT);
#             }
#         });
#     	Stream<A> sInitial = sSpark.<A>snapshot(this);
#         return sInitial.merge(updates(), new Lambda2<A,A,A>() {
#             public A apply(A left, A right) { return right; }
#         });
#     }
# 
#     /**
#      * Transform the cell's value according to the supplied function, so the returned Cell
#      * always reflects the value of the function applied to the input Cell's value.
#      * @param f Function to apply to convert the values. It must be <em>referentially transparent</em>.
#      */
# 	public final <B> Cell<B> map(final Lambda1<A,B> f)
    def map(self, f: Callable[[A], B]) -> "Cell[B]":
        return Transaction._apply(
            lambda trans: self \
                ._updates() \
                .map(f) \
                ._hold_lazy(trans, self._sample_lazy(trans).lift(f))
        )
# 	{
# 		return Transaction.apply(new Lambda1<Transaction, Cell<B>>() {
# 			public Cell<B> apply(Transaction trans) {
#                 return updates().map(f).holdLazy(trans, sampleLazy(trans).map(f));
#             }
#         });
# 	}
# 
# 	/**
# 	 * Lift a binary function into cells, so the returned Cell always reflects the specified
# 	 * function applied to the input cells' values.
# 	 * @param fn Function to apply. It must be <em>referentially transparent</em>.
# 	 */
# 	public final <B,C> Cell<C> lift(Cell<B> b, final Lambda2<A,B,C> fn)
    def lift(self, b: "Cell[B]", f: Callable[[A,B],C]) -> "Cell[C]":
        """
        Lift a binary function into cells, so the returned Cell always
        reflects the specified function applied to the input cells' values.

        @param f Function to apply. It must be **referentially transparent**.
        """
# 	{
# 		Lambda1<A, Lambda1<B,C>> ffa = new Lambda1<A, Lambda1<B,C>>() {
# 			public Lambda1<B,C> apply(final A aa) {
# 				return new Lambda1<B,C>() {
# 					public C apply(B bb) {
# 						return fn.apply(aa,bb);
# 					}
# 				};
# 			}
# 		};
        bf = self.map(lambda aa: lambda bb: f(aa, bb))
        return Cell.apply(bf, b)
# 		Cell<Lambda1<B,C>> bf = this.map(ffa);
# 		return this.apply(bf, b);
# 	}
# 
# 	/**
# 	 * Lift a ternary function into cells, so the returned Cell always reflects the specified
# 	 * function applied to the input cells' values.
# 	 * @param fn Function to apply. It must be <em>referentially transparent</em>.
# 	 */
# 	public final <B,C,D> Cell<D> lift(Cell<B> b, Cell<C> c, final Lambda3<A,B,C,D> fn)
# 	{
# 		Lambda1<A, Lambda1<B, Lambda1<C,D>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C,D>>>() {
# 			public Lambda1<B, Lambda1<C,D>> apply(final A aa) {
# 				return new Lambda1<B, Lambda1<C,D>>() {
# 					public Lambda1<C,D> apply(final B bb) {
# 						return new Lambda1<C,D>() {
# 							public D apply(C cc) {
# 								return fn.apply(aa,bb,cc);
# 							}
# 						};
# 					}
# 				};
# 			}
# 		};
# 		Cell<Lambda1<B, Lambda1<C, D>>> bf = this.map(ffa);
# 		return this.apply(apply(bf, b), c);
# 	}
# 
# 	/**
# 	 * Lift a quaternary function into cells, so the returned Cell always reflects the specified
# 	 * function applied to the input cells' values.
# 	 * @param fn Function to apply. It must be <em>referentially transparent</em>.
# 	 */
# 	public final <B,C,D,E> Cell<E> lift(Cell<B> b, Cell<C> c, Cell<D> d, final Lambda4<A,B,C,D,E> fn)
# 	{
# 		Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D,E>>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D,E>>>>() {
# 			public Lambda1<B, Lambda1<C, Lambda1<D,E>>> apply(final A aa) {
# 				return new Lambda1<B, Lambda1<C, Lambda1<D,E>>>() {
# 					public Lambda1<C, Lambda1<D, E>> apply(final B bb) {
# 						return new Lambda1<C, Lambda1<D,E>>() {
# 							public Lambda1<D,E> apply(final C cc) {
#                                 return new Lambda1<D, E>() {
#                                     public E apply(D dd) {
#                                         return fn.apply(aa,bb,cc,dd);
#                                     }
#                                 };
# 							}
# 						};
# 					}
# 				};
# 			}
# 		};
# 		Cell<Lambda1<B, Lambda1<C, Lambda1<D, E>>>> bf = this.map(ffa);
# 		return this.apply(apply(apply(bf, b), c), d);
# 	}
# 
# 	/**
# 	 * Lift a 5-argument function into cells, so the returned Cell always reflects the specified
# 	 * function applied to the input cells' values.
# 	 * @param fn Function to apply. It must be <em>referentially transparent</em>.
# 	 */
# 	public final <B,C,D,E,F> Cell<F> lift(Cell<B> b, Cell<C> c, Cell<D> d, Cell<E> e, final Lambda5<A,B,C,D,E,F> fn)
# 	{
# 		Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D, Lambda1<E, F>>>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D,Lambda1<E, F>>>>>() {
# 			public Lambda1<B, Lambda1<C, Lambda1<D, Lambda1<E, F>>>> apply(final A aa) {
# 				return new Lambda1<B, Lambda1<C, Lambda1<D, Lambda1<E, F>>>>() {
# 					public Lambda1<C, Lambda1<D, Lambda1<E, F>>> apply(final B bb) {
# 						return new Lambda1<C, Lambda1<D, Lambda1<E, F>>>() {
# 							public Lambda1<D, Lambda1<E, F>> apply(final C cc) {
#                                 return new Lambda1<D, Lambda1<E, F>>() {
#                                     public Lambda1<E, F> apply(final D dd) {
#                                         return new Lambda1<E, F>() {
#                                             public F apply(E ee) {
#                                                 return fn.apply(aa,bb,cc,dd,ee);
#                                             }
#                                         };
#                                     }
#                                 };
# 							}
# 						};
# 					}
# 				};
# 			}
# 		};
# 		Cell<Lambda1<B, Lambda1<C, Lambda1<D, Lambda1<E, F>>>>> bf = this.map(ffa);
# 		return this.apply(apply(apply(apply(bf, b), c), d), e);
# 	}
# 
# 	/**
# 	 * Lift a 6-argument function into cells, so the returned Cell always reflects the specified
# 	 * function applied to the input cells' values.
# 	 * @param fn Function to apply. It must be <em>referentially transparent</em>.
# 	 */
# 	public final <B,C,D,E,F,G> Cell<G> lift(Cell<B> b, Cell<C> c, Cell<D> d, Cell<E> e, Cell<F> f, final Lambda6<A,B,C,D,E,F,G> fn)
# 	{
# 		Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D, Lambda1<E, Lambda1<F, G>>>>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D,Lambda1<E, Lambda1<F, G>>>>>>() {
# 			public Lambda1<B, Lambda1<C, Lambda1<D, Lambda1<E, Lambda1<F, G>>>>> apply(final A aa) {
# 				return new Lambda1<B, Lambda1<C, Lambda1<D, Lambda1<E, Lambda1<F, G>>>>>() {
# 					public Lambda1<C, Lambda1<D, Lambda1<E, Lambda1<F, G>>>> apply(final B bb) {
# 						return new Lambda1<C, Lambda1<D, Lambda1<E, Lambda1<F, G>>>>() {
# 							public Lambda1<D, Lambda1<E, Lambda1<F, G>>> apply(final C cc) {
#                                 return new Lambda1<D, Lambda1<E, Lambda1<F, G>>>() {
#                                     public Lambda1<E, Lambda1<F, G>> apply(final D dd) {
#                                         return new Lambda1<E, Lambda1<F, G>>() {
#                                             public Lambda1<F, G> apply(final E ee) {
#                                                 return new Lambda1<F, G>() {
#                                                     public G apply(final F ff) {
#                                                         return fn.apply(aa,bb,cc,dd,ee,ff);
#                                                     }
#                                                 };
#                                             }
#                                         };
#                                     }
#                                 };
# 							}
# 						};
# 					}
# 				};
# 			}
# 		};
# 		Cell<Lambda1<B, Lambda1<C, Lambda1<D, Lambda1<E, Lambda1<F, G>>>>>> bf = this.map(ffa);
# 		return this.apply(apply(apply(apply(apply(bf, b), c), d), e), f);
# 	}
# 
# 	/**
# 	 * Apply a value inside a cell to a function inside a cell. This is the
# 	 * primitive for all function lifting.
# 	 */
# 	public static <A,B> Cell<B> apply(final Cell<Lambda1<A,B>> bf, final Cell<A> ba)
    @staticmethod
    def apply(bf: "Cell[Callable[[A],B]]", ba: "Cell[A]") -> "Cell[B]":
        """
        Apply a value inside a cell to a function inside a cell. This is the
        primitive for all function lifting.
        """
# 	{
#     	return Transaction.apply(new Lambda1<Transaction, Cell<B>>() {
#     		public Cell<B> apply(Transaction trans0) {
        def handler(trans0: Transaction) -> "Cell[B]":
            out: StreamWithSend[B] = StreamWithSend()
#                 final StreamWithSend<B> out = new StreamWithSend<B>();
# 
#                 class ApplyHandler implements Handler<Transaction> {
            class ApplyHandler:
                def __init__(self) -> None:
                    self.f: Callable[[A],B] = None
                    self.f_present = False
                    self.a: A = None
                    self.a_present = False
                def run(self, trans1: Transaction) -> None:
                    trans1._prioritized(out._node,
                        lambda trans2: out._send(trans2, self.f(self.a)))
#                     ApplyHandler(Transaction trans0) {
#                     }
#                     Lambda1<A,B> f = null;
#                     boolean f_present = false;
#                     A a = null;
#                     boolean a_present = false;
#                     @Override
#                     public void run(Transaction trans1) {
#                         trans1.prioritized(out.node, new Handler<Transaction>() {
#                             public void run(Transaction trans2) {
#                                 out.send(trans2, f.apply(a));
#                             }
#                         });
#                     }
#                 }
# 
            out_target = out._node
            in_target = Node(0)
            node_target_: List[Target] = [None]
            in_target._link_to(lambda _: None, out_target, node_target_)
            node_target = node_target_[0]
            h = ApplyHandler()
#                 Node out_target = out.node;
#                 final Node in_target = new Node(0);
#                 Node.Target[] node_target_ = new Node.Target[1];
#                 in_target.linkTo(null, out_target, node_target_);
#                 final Node.Target node_target = node_target_[0];
#                 final ApplyHandler h = new ApplyHandler(trans0);

            def handler_f(trans1: Transaction, f: Callable[[A],B]) -> None:
                h.f = f
                h.f_present = True
                if h.a_present:
                    h.run(trans1)
            l1 = bf._value_stream(trans0)._listen(in_target, handler_f)
#                 Listener l1 = bf.value(trans0).listen_(in_target, new TransactionHandler<Lambda1<A,B>>() {
#                     public void run(Transaction trans1, Lambda1<A,B> f) {
#                         h.f = f;
#                         h.f_present = true;
#                         if (h.a_present)
#                             h.run(trans1);
#                     }
#                 });
            def handler_a(trans1: Transaction, a: A) -> None:
                h.a = a
                h.a_present = True
                if h.f_present:
                    h.run(trans1)
            l2 = ba._value_stream(trans0)._listen(in_target, handler_a)
#                 Listener l2 = ba.value(trans0).listen_(in_target, new TransactionHandler<A>() {
#                     public void run(Transaction trans1, A a) {
#                         h.a = a;
#                         h.a_present = true;
#                         if (h.f_present)
#                             h.run(trans1);
#                     }
#                 });
            return out \
                ._last_firing_only(trans0) \
                ._unsafe_add_cleanup(l1) \
                ._unsafe_add_cleanup(l2) \
                ._unsafe_add_cleanup(
                    Listener(lambda: in_target._unlink_to(node_target))) \
                .hold_lazy(
                    Lazy(lambda: bf._sample_no_trans()(ba._sample_no_trans())))
#                 return out.lastFiringOnly(trans0).unsafeAddCleanup(l1).unsafeAddCleanup(l2).unsafeAddCleanup(
#                     new Listener() {
#                         public void unlisten() {
#                             in_target.unlinkTo(node_target);
#                         }
#                     }).holdLazy(new Lazy<B>(new Lambda0<B>() {
#                         public B apply() {
#                             return bf.sampleNoTrans().apply(ba.sampleNoTrans());
#                         }
#                     }));
#             }
#         });
        return Transaction._apply(handler)
# 	}
# 
# 	/**
# 	 * Unwrap a cell inside another cell to give a time-varying cell implementation.
# 	 */
# 	public static <A> Cell<A> switchC(final Cell<Cell<A>> bba)
    @staticmethod
    def switch_cell(bba: "Cell[Cell[A]]") -> "Cell[A]":
        """
        Unwrap a cell inside another cell to give a time-varying cell
        implementation.
        """
# 	{
# 	    return Transaction.apply(new Lambda1<Transaction, Cell<A>>() {
# 	        public Cell<A> apply(Transaction trans0) {
        def helper(trans0: Transaction) -> "Cell[A]":
            za = bba.sample_lazy().lift(lambda ba: ba.sample())
#                 Lazy<A> za = bba.sampleLazy().map(new Lambda1<Cell<A>, A>() {
#                     public A apply(Cell<A> ba) {
#                         return ba.sample();
#                     }
#                 });
            out: StreamWithSend[A] = StreamWithSend()
#                 final StreamWithSend<A> out = new StreamWithSend<A>();
#                 TransactionHandler<Cell<A>> h = new TransactionHandler<Cell<A>>() {
            class _Handler:
#                     private Listener currentListener;
                def __init__(self) -> None:
                    self.current_listener: Listener = None
#                     @Override
#                     public void run(Transaction trans2, Cell<A> ba) {
                def __call__(self, trans2: Transaction, ba: Cell[A]) -> None:
                    # Note: If any switch takes place during a transaction,
                    # then the _last_firing_only() below will always cause
                    # a sample to be fetched from the one we just switched
                    # to. So anything from the old input cell that might
                    # have happened during this transaction will be suppressed.
                    if self.current_listener is not None:
                        self.current_listener.unlisten()
                    self.current_listener = ba \
                        ._value_stream(trans2) \
                        ._listen_internal(out._node, trans2, out._send, False)

#                         // Note: If any switch takes place during a transaction, then the
#                         // lastFiringOnly() below will always cause a sample to be fetched
#                         // from the one we just switched to. So anything from the old input cell
#                         // that might have happened during this transaction will be suppressed.
#                         if (currentListener != null)
#                             currentListener.unlisten();
#                         currentListener = ba.value(trans2).listen(out.node, trans2, new TransactionHandler<A>() {
#                             public void run(Transaction trans3, A a) {
#                                 out.send(trans3, a);
#                             }
#                         }, false);
#                     }
# 
#                     @Override
#                     protected void finalize() throws Throwable {
                def __del__(self) -> None:
                    if self.current_listener is not None:
                        self.current_listener.unlisten()
#                         if (currentListener != null)
#                             currentListener.unlisten();
#                     }
#                 };
            l1 = bba._value_stream(trans0)._listen(out._node, _Handler())
            return out \
                ._last_firing_only(trans0) \
                ._unsafe_add_cleanup(l1) \
                .hold_lazy(za)
#                 Listener l1 = bba.value(trans0).listen_(out.node, h);
#                 return out.lastFiringOnly(trans0).unsafeAddCleanup(l1).holdLazy(za);
#             }
#         });
        return Transaction._apply(helper)
# 	}
# 	
# 	/**
# 	 * Unwrap a stream inside a cell to give a time-varying stream implementation.
# 	 */
# 	public static <A> Stream<A> switchS(final Cell<Stream<A>> bea)
    @staticmethod
    def switch_stream(bea: "Cell[Stream[A]]") -> Stream[A]:
        """
        Unwrap a stream inside a cell to give a time-varying stream
        implementation.
        """
        return Transaction._apply(
            lambda trans: Cell._switch_stream(trans, bea))
# 	{
#         return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
#         	public Stream<A> apply(final Transaction trans) {
#                 return switchS(trans, bea);
#         	}
#         });
#     }
# 
# 	private static <A> Stream<A> switchS(final Transaction trans1, final Cell<Stream<A>> bea)
    @staticmethod
    def _switch_stream(
            trans1: Transaction,
            bea: "Cell[Stream[A]]") -> Stream[A]:
        out: StreamWithSend[A] = StreamWithSend()
        h2 = out._send
# 	{
#         final StreamWithSend<A> out = new StreamWithSend<A>();
#         final TransactionHandler<A> h2 = new TransactionHandler<A>() {
#         	public void run(Transaction trans2, A a) {
# 	            out.send(trans2, a);
# 	        }
#         };
#         TransactionHandler<Stream<A>> h1 = new TransactionHandler<Stream<A>>() {
        class _Handler:
#             private Listener currentListener = bea.sampleNoTrans().listen(out.node, trans1, h2, false);
            def __init__(self) -> None:
                self.current_listener = bea \
                    ._sample_no_trans() \
                    ._listen_internal(out._node, trans1, h2, False)
# 
#             @Override
#             public void run(final Transaction trans2, final Stream<A> ea) {
            def __call__(self, trans2: Transaction, ea: Stream[A]) -> None:
                def run() -> None:
                    if self.current_listener is not None:
                        self.current_listener.unlisten()
                    self.current_listener = ea._listen_internal(
                        out._node, trans2, h2, True)
                trans2.last(run)
#                 trans2.last(new Runnable() {
#                 	public void run() {
# 	                    if (currentListener != null)
# 	                        currentListener.unlisten();
# 	                    currentListener = ea.listen(out.node, trans2, h2, true);
# 	                }
#                 });
#             }
# 
#             @Override
#             protected void finalize() throws Throwable {
            def __del__(self) -> None:
                if self.current_listener is not None:
                    self.current_listener.unlisten()
#                 if (currentListener != null)
#                     currentListener.unlisten();
#             }
#         };
        h1 = _Handler()
        l1 = bea._updates()._listen_internal(out._node, trans1, h1, False)
        return out._unsafe_add_cleanup(l1)
#         Listener l1 = bea.updates().listen(out.node, trans1, h1, false);
#         return out.unsafeAddCleanup(l1);
# 	}
# 
# 	@Override
# 	protected void finalize() throws Throwable {
# 	    if (cleanup != null)
#             cleanup.unlisten();
# 	}
# 
# 	/**
# 	 * Listen for updates to the value of this cell. This is the observer pattern. The
# 	 * returned {@link Listener} has a {@link Listener#unlisten()} method to cause the
# 	 * listener to be removed. This is an OPERATIONAL mechanism is for interfacing between
# 	 * the world of I/O and for FRP.
# 	 * @param action The handler to execute when there's a new value.
# 	 *   You should make no assumptions about what thread you are called on, and the
# 	 *   handler should not block. You are not allowed to use {@link CellSink#send(Object)}
# 	 *   or {@link StreamSink#send(Object)} in the handler.
# 	 *   An exception will be thrown, because you are not meant to use this to create
# 	 *   your own primitives.
#      */
# 	public final Listener listen(final Handler<A> action) {
    def listen(self, action: Callable[[A],None]) -> Listener:
        """
        Listen for updates to the value of this cell. This is the observer
        pattern. The returned `Listener` has a `Listener.unlisten` method
        to cause the listener to be removed. This is an OPERATIONAL
        mechanism is for interfacing between the world of I/O and for FRP.

        @param action The handler to execute when there's a new value.
            You should make no assumptions about what thread you are called
            on, and the handler should not block. You are not allowed
            to use `CellSink.send` or `StreamSink.send` in the handler.
            An exception will be thrown, because you are not meant to use
            this to create your own primitives.
        """
        return Transaction._apply(
            lambda trans: self._value_stream(trans).listen(action))
#         return Transaction.apply(new Lambda1<Transaction, Listener>() {
#         	public Listener apply(final Transaction trans) {
#                 return value(trans).listen(action);
# 			}
# 		});
# 	}
# 
# 	/**
# 	 * A variant of {@link #listen(Handler)} that will deregister the listener automatically
# 	 * if the listener is garbage collected. With {@link #listen(Handler)}, the listener is
# 	 * only deregistered if {@link Listener#unlisten()} is called explicitly.
# 	 */
# 	public final Listener listenWeak(final Handler<A> action) {
# 		return Transaction.apply(new Lambda1<Transaction, Listener>() {
# 			public Listener apply(final Transaction trans) {
# 				return value(trans).listenWeak(action);
# 			}
# 		});
# 	}
# 
# 	/**
# 	 * Lift a binary function into cells, so the returned Cell always reflects the specified
# 	 * function applied to the input cells' values.
# 	 * @param fn Function to apply. It must be <em>referentially transparent</em>.
# 	 * @deprecated As of release 1.1.0, replaced by {@link #lift(Cell,Lambda2)}
# 	 */
#     @Deprecated
# 	public static final <A,B,C> Cell<C> lift(final Lambda2<A,B,C> fn, Cell<A> a, Cell<B> b)
# 	{
# 		return a.lift(b, fn);
# 	}
# 
# 	/**
# 	 * Lift a ternary function into cells, so the returned Cell always reflects the specified
# 	 * function applied to the input cells' values.
# 	 * @param fn Function to apply. It must be <em>referentially transparent</em>.
# 	 * @deprecated As of release 1.1.0, replaced by {@link #lift(Cell,Cell,Lambda3)}
# 	 */
#     @Deprecated
# 	public static final <A,B,C,D> Cell<D> lift(final Lambda3<A,B,C,D> fn, Cell<A> a, Cell<B> b, Cell<C> c)
# 	{
# 		return a.lift(b, c, fn);
# 	}
# 
# 	/**
# 	 * Lift a quaternary function into cells, so the returned Cell always reflects the specified
# 	 * function applied to the input cells' values.
# 	 * @param fn Function to apply. It must be <em>referentially transparent</em>.
# 	 * @deprecated As of release 1.1.0, replaced by {@link #lift(Cell,Cell,Cell,Lambda4)}
# 	 */
#     @Deprecated
# 	public static final <A,B,C,D,E> Cell<E> lift(final Lambda4<A,B,C,D,E> fn, Cell<A> a, Cell<B> b, Cell<C> c, Cell<D> d)
# 	{
# 		return a.lift(b, c, d, fn);
# 	}
# 
# 	/**
# 	 * Lift a 5-argument function into cells, so the returned Cell always reflects the specified
# 	 * function applied to the input cells' values.
# 	 * @param fn Function to apply. It must be <em>referentially transparent</em>.
# 	 * @deprecated As of release 1.1.0, replaced by {@link #lift(Cell,Cell,Cell,Cell,Lambda5)}
# 	 */
#     @Deprecated
# 	public static final <A,B,C,D,E,F> Cell<F> lift(final Lambda5<A,B,C,D,E,F> fn, Cell<A> a, Cell<B> b, Cell<C> c, Cell<D> d, Cell<E> e)
# 	{
# 		return a.lift(b, c, d, e, fn);
# 	}
# 
# 	/**
# 	 * Lift a 6-argument function into cells, so the returned Cell always reflects the specified
# 	 * function applied to the input cells' values.
# 	 * @param fn Function to apply. It must be <em>referentially transparent</em>.
# 	 * @deprecated As of release 1.1.0, replaced by {@link #lift(Cell,Cell,Cell,Cell,Cell,Lambda6)}
# 	 */
#     @Deprecated
# 	public static final <A,B,C,D,E,F,G> Cell<G> lift(final Lambda6<A,B,C,D,E,F,G> fn, Cell<A> a, Cell<B> b, Cell<C> c, Cell<D> d, Cell<E> e, Cell<F> f)
# 	{
# 		return a.lift(b, c, d, e, f, fn);
# 	}
# }
# 

# package nz.sodium;
# 
# /**
#  * A cell that allows values to be pushed into it, acting as an interface between the
#  * world of I/O and the world of FRP. Code that exports CellSinks for read-only use
#  * should downcast to {@link Cell}.
#  */
# public final class CellSink<A> extends Cell<A> {
class CellSink(Cell[A]):
    """
    A cell that allows values to be pushed into it, acting as an interface
    between the world of I/O and the world of FRP. Code that exports
    CellSinks for read-only use should downcast to `Cell`.
    """
#     /**
#      * Construct a writable cell with the specified initial value. If multiple values are
#      * sent in the same transaction, the last one is used.
#      */
#     public CellSink(A initValue) {
#     	super(new StreamSink<A>(), initValue);
#     }
# 
#     /**
#      * Construct a writable cell with the specified initial value. If multiple values are
#      * sent in the same transaction, the specified function is used to combine them.
#      */
#     public CellSink(A initValue, Lambda2<A,A,A> f) {
    def __init__(self, init_value: A, f: Callable[[A,A],A] = None) -> None:
        """
        Construct a writable cell with the specified initial value.
        If multiple values are sent in the same transaction, the specified
        function is used to combine them. If the function isn't provided,
        `send()` throws an exception when called multiple times from the
        same transaction.
        """
        super().__init__(StreamSink(f), init_value)
#     	super(new StreamSink<A>(f), initValue);
#     }
# 
#     /**
#      * Send a value, modifying the value of the cell. send(A) may not be used inside
#      * handlers registered with {@link Stream#listen(Handler)} or {@link Cell#listen(Handler)}.
#      * An exception will be thrown, because CellSink is for interfacing I/O to FRP only.
#      * You are not meant to use this to define your own primitives.
#      * @param a Value to push into the cell.
#      */
#     public void send(A a)
    def send(self, a: A) -> None:
        """
        Send a value, modifying the value of the cell. send(A) may not be
        used inside handlers registered with `Stream.listen()` or
        `Cell.listen()`. An exception will be thrown, because CellSink is
        for interfacing I/O to FRP only. You are not meant to use this
        to define your own primitives.

        @param a Value to push into the cell.
        """
        self._stream.send(a)
#     {
#         ((StreamSink<A>)str).send(a);
#     }
# }

# class LazyCell<A> extends Cell<A> {
class LazyCell(Cell[A]):
#     LazyCell(final Stream<A> event, final Lazy<A> lazyInitValue) {
    def __init__(self, event: Stream[A], lazy_init_value: Lazy[A]) -> None:
        super().__init__(event, None)
        self._lazy_init_value = lazy_init_value
#         super(event, null);
#         this.lazyInitValue = lazyInitValue;
#     }
# 
#     @Override
#     A sampleNoTrans()
    def _sample_no_trans(self) -> A:
        if (self._value is None) and (self._lazy_init_value is not None):
            self._value = self._lazy_init_value.get()
            self._lazy_init_value = None
        return self._value
#     {
#         if (value == null && lazyInitValue != null) {
#             value = lazyInitValue.get();
#             lazyInitValue = null;
#         }
#         return value;
#     }
# }

# /**
#  * A forward reference for a {@link Cell} equivalent to the Cell that is referenced.
#  */
# public final class CellLoop<A> extends LazyCell<A> {
class CellLoop(LazyCell[A]):
    """
    A forward reference for a `Cell` equivalent to the Cell that is
    referenced.
    """
#     public CellLoop() {
    def __init__(self) -> None:
        super().__init__(StreamLoop(), None)
#     	super(new StreamLoop<A>(), null);
#     }
# 
#     /**
#      * Resolve the loop to specify what the CellLoop was a forward reference to. It
#      * must be invoked inside the same transaction as the place where the CellLoop is used.
#      * This requires you to create an explicit transaction with {@link Transaction#run(Lambda0)}
#      * or {@link Transaction#runVoid(Runnable)}.
#      */
#     public void loop(final Cell<A> a_out)
    def loop(self, a_out: Cell[A]) -> None:
        """
        Resolve the loop to specify what the CellLoop was a forward
        reference to. It must be invoked inside the same transaction as
        the place where the CellLoop is used. This requires you to create
        an explicit transaction with `Transaction.run()`.
        """
        def handler(trans: Transaction) -> Unit:
            self._stream.loop(a_out._updates())
            self._lazy_init_value = a_out._sample_lazy(trans)
            return UNIT
        Transaction._apply(handler)
#     {
#         final CellLoop<A> me = this;
#         Transaction.apply(new Lambda1<Transaction, Unit>() {
#         	public Unit apply(final Transaction trans) {
#                 ((StreamLoop<A>)me.str).loop(a_out.updates());
#                 me.lazyInitValue = a_out.sampleLazy(trans);
#                 return Unit.UNIT;
#             }
#         });
#     }
# 
#     @Override
#     A sampleNoTrans()
    def _sample_no_trans(self) -> A:
        if not self._stream._assigned:
            raise RuntimeError("CellLoop sampled before it was looped")
        return super()._sample_no_trans()
#     {
#         if (!((StreamLoop<A>)str).assigned)
#             throw new RuntimeException("CellLoop sampled before it was looped");
#         return super.sampleNoTrans();
#     }
# }
