package sodium;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

public class Stream<A> {
	private static final class ListenerImplementation<A> extends Listener {
		/**
		 * It's essential that we keep the listener alive while the caller holds
		 * the Listener, so that the finalizer doesn't get triggered.
		 */
		private final Stream<A> event;
		private final TransactionHandler<A> action;
		private final Node.Target target;

		private ListenerImplementation(Stream<A> event, TransactionHandler<A> action, Node.Target target) {
			this.event = event;
			this.action = action;
			this.target = target;
		}

		public void unlisten() {
		    synchronized (Transaction.listenersLock) {
                event.node.unlinkTo(target);
            }
		}
	}

	final Node node;
	protected final List<Listener> finalizers;
	protected final List<A> firings;

	/**
	 * An event that never fires.
	 */
	public Stream() {
	    this.node = new Node(0L);
	    this.finalizers = new ArrayList<Listener>();
	    this.firings = new ArrayList<A>();
	}

	private Stream(Node node, List<Listener> finalizers, List<A> firings) {
	    this.node = node;
	    this.finalizers = finalizers;
        this.firings = firings;
	}

	/**
	 * Listen for firings of this event. The returned Listener has an unlisten()
	 * method to cause the listener to be removed. This is the observer pattern.
     */
	public final Listener listen(final Handler<A> action) {
		return listen_(Node.NULL, new TransactionHandler<A>() {
			public void run(Transaction trans2, A a) {
				action.run(a);
			}
		});
	}

	final Listener listen_(final Node target, final TransactionHandler<A> action) {
		return Transaction.apply(new Lambda1<Transaction, Listener>() {
			public Listener apply(Transaction trans1) {
				return listen(target, trans1, action, false);
			}
		});
	}

	@SuppressWarnings("unchecked")
	final Listener listen(Node target, Transaction trans, TransactionHandler<A> action, boolean suppressEarlierFirings) {
	    Node.Target[] node_target_ = new Node.Target[1];
        synchronized (Transaction.listenersLock) {
            if (node.linkTo((TransactionHandler<Unit>)action, target, node_target_))
                trans.toRegen = true;
        }
        Node.Target node_target = node_target_[0];
        final List<A> firings = new ArrayList<A>(this.firings);
        if (!suppressEarlierFirings && !firings.isEmpty())
            trans.prioritized(target, new Handler<Transaction>() {
                public void run(Transaction trans2) {
                    // Anything sent already in this transaction must be sent now so that
                    // there's no order dependency between send and listen.
                    for (A a : firings) {
                        Transaction.inCallback++;
                        try {  // Don't allow transactions to interfere with Sodium
                               // internals.
                            action.run(trans, a);
                        } catch (Throwable t) {
                            t.printStackTrace();
                        }
                        finally {
                            Transaction.inCallback--;
                        }
                    }
                }
            });
		return new ListenerImplementation<A>(this, action, node_target);
	}

    /**
     * Transform the event's value according to the supplied function.
     */
	public final <B> Stream<B> map(final Lambda1<A,B> f)
	{
	    final Stream<A> ev = this;
	    final StreamSink<B> out = new StreamSink<B>();
        Listener l = listen_(out.node, new TransactionHandler<A>() {
        	public void run(Transaction trans2, A a) {
	            out.send(trans2, f.apply(a));
	        }
        });
        return out.unsafeAddCleanup(l);
	}

	/**
	 * Create a behavior with the specified initial value, that gets updated
     * by the values coming through the event. The 'current value' of the behavior
     * is notionally the value as it was 'at the start of the transaction'.
     * That is, state updates caused by event firings get processed at the end of
     * the transaction.
     */
	public final Cell<A> hold(final A initValue) {
		return Transaction.apply(new Lambda1<Transaction, Cell<A>>() {
			public Cell<A> apply(Transaction trans) {
			    return new Cell<A>(lastFiringOnly(trans), initValue);
			}
		});
	}

	public final Cell<A> holdLazy(final Lazy<A> initValue) {
		return Transaction.apply(new Lambda1<Transaction, Cell<A>>() {
			public Cell<A> apply(Transaction trans) {
			    return new LazyCell<A>(lastFiringOnly(trans), initValue);
			}
		});
	}

	/**
	 * Variant of snapshot that throws away the event's value and captures the behavior's.
	 */
	public final <B> Stream<B> snapshot(Cell<B> beh)
	{
	    return snapshot(beh, new Lambda2<A,B,B>() {
	    	public B apply(A a, B b) {
	    		return b;
	    	}
	    });
	}

	/**
	 * Sample the behavior at the time of the event firing. Note that the 'current value'
     * of the behavior that's sampled is the value as at the start of the transaction
     * before any state changes of the current transaction are applied through 'hold's.
     */
	public final <B,C> Stream<C> snapshot(final Cell<B> b, final Lambda2<A,B,C> f)
	{
	    final Stream<A> ev = this;
		final StreamSink<C> out = new StreamSink<C>();
        Listener l = listen_(out.node, new TransactionHandler<A>() {
        	public void run(Transaction trans2, A a) {
	            out.send(trans2, f.apply(a, b.sampleNoTrans()));
	        }
        });
        return out.unsafeAddCleanup(l);
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
	public Stream<A> merge(final Stream<A> eb)
	{
	    return Stream.<A>merge(this, eb);
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
	private static <A> Stream<A> merge(final Stream<A> ea, final Stream<A> eb)
	{
	    final StreamSink<A> out = new StreamSink<A>();
        final Node left = new Node(0);
        final Node right = out.node;
        Node.Target[] node_target_ = new Node.Target[1];
        left.linkTo(null, right, node_target_);
        Node.Target node_target = node_target_[0];
        TransactionHandler<A> h = new TransactionHandler<A>() {
        	public void run(Transaction trans, A a) {
	            out.send(trans, a);
	        }
        };
        Listener l1 = ea.listen_(left, h);
        Listener l2 = eb.listen_(right, h);
        return out.unsafeAddCleanup(l1).unsafeAddCleanup(l2).unsafeAddCleanup(new Listener() {
            public void unlisten() {
                left.unlinkTo(node_target);
            }
        });
	}

	/**
	 * Push this event occurrence onto a new transaction. Same as split() but works
     * on a single value.
	 */
	public final Stream<A> defer()
	{
	    final StreamSink<A> out = new StreamSink<A>();
	    Listener l1 = listen_(out.node, new TransactionHandler<A>() {
	        public void run(Transaction trans, final A a) {
	            trans.post(new Runnable() {
                    public void run() {
                        Transaction trans = new Transaction();
                        try {
                            out.send(trans, a);
                        } finally {
                            trans.close();
                        }
                    }
	            });
	        }
	    });
	    return out.unsafeAddCleanup(l1);
	}

	/**
	 * Push each event occurrence in the list onto a new transaction.
	 */
    public static final <A, C extends Collection<A>> Stream<A> split(Stream<C> s)
    {
	    final StreamSink<A> out = new StreamSink<A>();
	    Listener l1 = s.listen_(out.node, new TransactionHandler<C>() {
	        public void run(Transaction trans, final C as) {
	            trans.post(new Runnable() {
                    public void run() {
                        for (A a : as) {
                            Transaction trans = new Transaction();
                            try {
                                out.send(trans, a);
                            } finally {
                                trans.close();
                            }
                        }
                    }
	            });
	        }
	    });
	    return out.unsafeAddCleanup(l1);
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
	public final Stream<A> coalesce(final Lambda2<A,A,A> f)
	{
	    return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
	    	public Stream<A> apply(Transaction trans) {
	    		return coalesce(trans, f);
	    	}
	    });
	}

	final Stream<A> coalesce(Transaction trans1, final Lambda2<A,A,A> f)
	{
	    final Stream<A> ev = this;
	    final StreamSink<A> out = new StreamSink<A>();
        TransactionHandler<A> h = new CoalesceHandler<A>(f, out);
        Listener l = listen(out.node, trans1, h, false);
        return out.unsafeAddCleanup(l);
    }

    /**
     * Clean up the output by discarding any firing other than the last one. 
     */
    final Stream<A> lastFiringOnly(Transaction trans)
    {
        return coalesce(trans, new Lambda2<A,A,A>() {
        	public A apply(A first, A second) { return second; }
        });
    }

    /**
     * Merge two streams of events of the same type, combining simultaneous
     * event occurrences.
     *
     * In the case where multiple event occurrences are simultaneous (i.e. all
     * within the same transaction), they are combined using the same logic as
     * 'coalesce'.
     */
    public Stream<A> merge(Stream<A> eb, Lambda2<A,A,A> f)
    {
        return merge(eb).coalesce(f);
    }

    /**
     * Only keep event occurrences for which the predicate returns true.
     */
    public final Stream<A> filter(final Lambda1<A,Boolean> f)
    {
        final Stream<A> ev = this;
        final StreamSink<A> out = new StreamSink<A>();
        Listener l = listen_(out.node, new TransactionHandler<A>() {
        	public void run(Transaction trans2, A a) {
	            if (f.apply(a)) out.send(trans2, a);
	        }
        });
        return out.unsafeAddCleanup(l);
    }

    /**
     * Filter out any event occurrences whose value is a Java null pointer.
     */
    public final Stream<A> filterNotNull()
    {
        return filter(new Lambda1<A,Boolean>() {
        	public Boolean apply(A a) { return a != null; }
        });
    }

    /**
     * Filter the empty values out, and strip the Optional wrapper from the present ones.
     */
    public static final <A> Stream<A> filterOptional(final Stream<Optional<A>> ev)
    {
        final StreamSink<A> out = new StreamSink<A>();
        Listener l = ev.listen_(out.node, new TransactionHandler<Optional<A>>() {
        	public void run(Transaction trans2, Optional<A> oa) {
	            if (oa.isPresent()) out.send(trans2, oa.get());
	        }
        });
        return out.unsafeAddCleanup(l);
    }

    /**
     * Let event occurrences through only when the behavior's value is True.
     * Note that the behavior's value is as it was at the start of the transaction,
     * that is, no state changes from the current transaction are taken into account.
     */
    public final Stream<A> gate(Cell<Boolean> bPred)
    {
        return snapshot(bPred, new Lambda2<A,Boolean,A>() {
        	public A apply(A a, Boolean pred) { return pred ? a : null; }
        }).filterNotNull();
    }

    /**
     * Transform an event with a generalized state loop (a mealy machine). The function
     * is passed the input and the old state and returns the new state and output value.
     */
    public final <B,S> Stream<B> collect(final S initState, final Lambda2<A, S, Tuple2<B, S>> f)
    {
        return collectLazy(new Lazy<S>(initState), f);
    }

    /**
     * Transform an event with a generalized state loop (a mealy machine). The function
     * is passed the input and the old state and returns the new state and output value.
     */
    public final <B,S> Stream<B> collectLazy(final Lazy<S> initState, final Lambda2<A, S, Tuple2<B, S>> f)
    {
        return Transaction.<Stream<B>>run(() -> {
            final Stream<A> ea = this;
            StreamLoop<S> es = new StreamLoop<S>();
            Cell<S> s = es.holdLazy(initState);
            Stream<Tuple2<B,S>> ebs = ea.snapshot(s, f);
            Stream<B> eb = ebs.map(new Lambda1<Tuple2<B,S>,B>() {
                public B apply(Tuple2<B,S> bs) { return bs.a; }
            });
            Stream<S> es_out = ebs.map(new Lambda1<Tuple2<B,S>,S>() {
                public S apply(Tuple2<B,S> bs) { return bs.b; }
            });
            es.loop(es_out);
            return eb;
        });
    }

    /**
     * Accumulate on input event, outputting the new state each time.
     */
    public final <S> Cell<S> accum(final S initState, final Lambda2<A, S, S> f)
    {
        return accumLazy(new Lazy<S>(initState), f);
    }

    /**
     * Accumulate on input event, outputting the new state each time.
     * Variant that takes a lazy initial state.
     */
    public final <S> Cell<S> accumLazy(final Lazy<S> initState, final Lambda2<A, S, S> f)
    {
        return Transaction.<Cell<S>>run(() -> {
            final Stream<A> ea = this;
            StreamLoop<S> es = new StreamLoop<S>();
            Cell<S> s = es.holdLazy(initState);
            Stream<S> es_out = ea.snapshot(s, f);
            es.loop(es_out);
            return es_out.holdLazy(initState);
        });
    }

    /**
     * Throw away all event occurrences except for the first one.
     */
    public final Stream<A> once()
    {
        // This is a bit long-winded but it's efficient because it deregisters
        // the listener.
        final Stream<A> ev = this;
        final Listener[] la = new Listener[1];
        final StreamSink<A> out = new StreamSink<A>();
        la[0] = ev.listen_(out.node, new TransactionHandler<A>() {
        	public void run(Transaction trans, A a) {
	            if (la[0] != null) {
                    out.send(trans, a);
	                la[0].unlisten();
	                la[0] = null;
	            }
	        }
        });
        return out.unsafeAddCleanup(la[0]);
    }

    Stream<A> unsafeAddCleanup(Listener cleanup)
    {
        finalizers.add(cleanup);
        return this;
    }

    public Stream<A> addCleanup(Listener cleanup) {
        List<Listener> fsNew = new ArrayList<Listener>(finalizers);
        fsNew.add(cleanup);
        return new Stream<A>(node, fsNew, firings);
    }

	@Override
	protected void finalize() throws Throwable {
		for (Listener l : finalizers)
			l.unlisten();
	}
}

class CoalesceHandler<A> implements TransactionHandler<A>
{
	public CoalesceHandler(Lambda2<A,A,A> f, StreamSink<A> out)
	{
	    this.f = f;
	    this.out = out;
	}
	private Lambda2<A,A,A> f;
	private StreamSink<A> out;
    private boolean accumValid = false;
    private A accum;
    @Override
    public void run(Transaction trans1, A a) {
        if (accumValid)
            accum = f.apply(accum, a);
        else {
        	final CoalesceHandler<A> thiz = this;
            trans1.prioritized(out.node, new Handler<Transaction>() {
            	public void run(Transaction trans2) {
                    out.send(trans2, thiz.accum);
                    thiz.accumValid = false;
                    thiz.accum = null;
                }
            });
            accum = a;
            accumValid = true;
        }
    }
}

