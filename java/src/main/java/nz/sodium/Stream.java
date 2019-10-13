package nz.sodium;

import java.util.ArrayList;
import java.util.List;
import java.util.HashSet;
import java.util.Optional;
import java.util.Vector;

/**
 * Represents a stream of discrete events/firings containing values of type A. 
 */
public class Stream<A> {
	private static final class ListenerImplementation<A> extends Listener {
		/**
		 * It's essential that we keep the listener alive while the caller holds
		 * the Listener, so that the finalizer doesn't get triggered.
		 */
		private Stream<A> event;
		/**
		 * It's also essential that we keep the action alive, since the node uses
		 * a weak reference.
		 */
		private TransactionHandler<A> action;
		private Node.Target target;

		private ListenerImplementation(Stream<A> event, TransactionHandler<A> action, Node.Target target) {
			this.event = event;
			this.action = action;
			this.target = target;
		}

		public void unlisten() {
		    synchronized (Transaction.listenersLock) {
		        if (this.event != null) {
                    event.node.unlinkTo(target);
                    this.event = null;
                    this.action = null;
                    this.target = null;
                }
            }
		}
	}

	final Node node;
	final List<Listener> finalizers;
	final List<A> firings;

	/**
	 * A stream that never fires.
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

    static HashSet<Listener> keepListenersAlive = new HashSet<Listener>();

	/**
	 * Listen for events/firings on this stream. This is the observer pattern. The
	 * returned {@link Listener} has a {@link Listener#unlisten()} method to cause the
	 * listener to be removed. This is an OPERATIONAL mechanism is for interfacing between
	 * the world of I/O and for FRP.
	 * @param handler The handler to execute when there's a new value.
	 *   You should make no assumptions about what thread you are called on, and the
	 *   handler should not block. You are not allowed to use {@link CellSink#send(Object)}
	 *   or {@link StreamSink#send(Object)} in the handler.
	 *   An exception will be thrown, because you are not meant to use this to create
	 *   your own primitives.
     */
	public final Listener listen(final Handler<A> handler) {
        final Listener l0 = listenWeak(handler);
        Listener l = new Listener() {
            public void unlisten() {
                l0.unlisten();
                synchronized (keepListenersAlive) {
                    keepListenersAlive.remove(this);
                }
            }
        };
        synchronized (keepListenersAlive) {
            keepListenersAlive.add(l);
        }
        return l;
	}

    /**
     * A variant of {@link #listen(Handler)} that handles the first event and then
     * automatically deregisters itself. This is useful for implementing things that
     * work like promises.
     */
    public final Listener listenOnce(final Handler<A> handler) {
        final Listener[] lRef = new Listener[1];
        lRef[0] = listen(new Handler<A>() {
            public void run(A a) {
                lRef[0].unlisten();
                handler.run(a);
            }
        });
        return lRef[0];
    }

	final Listener listen_(final Node target, final TransactionHandler<A> action) {
		return Transaction.apply(new Lambda1<Transaction, Listener>() {
			public Listener apply(Transaction trans1) {
				return listen(target, trans1, action, false);
			}
		});
	}

    /**
     * A variant of {@link #listen(Handler)} that will deregister the listener automatically
     * if the listener is garbage collected. With {@link #listen(Handler)}, the listener is
     * only deregistered if {@link Listener#unlisten()} is called explicitly.
     * <P>
     * This method should be used for listeners that are to be passed to {@link Stream#addCleanup(Listener)}
     * to ensure that things don't get kept alive when they shouldn't.
     */
    public final Listener listenWeak(final Handler<A> action) {
		return listen_(Node.NULL, new TransactionHandler<A>() {
			public void run(Transaction trans2, A a) {
				action.run(a);
			}
		});
    }

	@SuppressWarnings("unchecked")
	final Listener listen(Node target, Transaction trans, final TransactionHandler<A> action, boolean suppressEarlierFirings) {
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
                            action.run(trans2, a);
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
     * Transform the stream's event values according to the supplied function, so the returned
     * Stream's event values reflect the value of the function applied to the input
     * Stream's event values.
     * @param f Function to apply to convert the values. It may construct FRP logic or use
     *    {@link Cell#sample()} in which case it is equivalent to {@link Stream#snapshot(Cell)}ing the
     *    cell. Apart from this the function must be <em>referentially transparent</em>.
     */
	public final <B> Stream<B> map(final Lambda1<A,B> f)
	{
	    final Stream<A> ev = this;
	    final StreamWithSend<B> out = new StreamWithSend<B>();
        Listener l = listen_(out.node, new TransactionHandler<A>() {
        	public void run(Transaction trans2, A a) {
	            out.send(trans2, f.apply(a));
	        }
        });
        return out.unsafeAddCleanup(l);
	}

    /**
     * Transform the stream's event values into the specified constant value.
     * @param b Constant value.
     */
	public final <B> Stream<B> mapTo(final B b)
	{
		return this.<B>map(new Lambda1<A, B>() {
			public B apply(A a) {
			    return b;
			}
		});
	}

	/**
	 * Create a {@link Cell} with the specified initial value, that is updated
     * by this stream's event values.
     * <p>
     * There is an implicit delay: State updates caused by event firings don't become
     * visible as the cell's current value as viewed by {@link Stream#snapshot(Cell, Lambda2)}
     * until the following transaction. To put this another way,
     * {@link Stream#snapshot(Cell, Lambda2)} always sees the value of a cell as it was before
     * any state changes from the current transaction.
     */
	public final Cell<A> hold(final A initValue) {
		return Transaction.apply(new Lambda1<Transaction, Cell<A>>() {
			public Cell<A> apply(Transaction trans) {
			    return new Cell<A>(Stream.this, initValue);
			}
		});
	}

	/**
	 * A variant of {@link #hold(Object)} with an initial value captured by {@link Cell#sampleLazy()}.
	 */
	public final Cell<A> holdLazy(final Lazy<A> initValue) {
		return Transaction.apply(new Lambda1<Transaction, Cell<A>>() {
			public Cell<A> apply(Transaction trans) {
			    return holdLazy(trans, initValue);
			}
		});
	}

	final Cell<A> holdLazy(Transaction trans, final Lazy<A> initValue) {
	    return new LazyCell<A>(this, initValue);
	}

	/**
	 * Variant of {@link #snapshot(Cell, Lambda2)} that captures the cell's value
	 * at the time of the event firing, ignoring the stream's value.
	 */
	public final <B> Stream<B> snapshot(Cell<B> c)
	{
	    return snapshot(c, new Lambda2<A,B,B>() {
	    	public B apply(A a, B b) {
	    		return b;
	    	}
	    });
	}

	/**
	 * Return a stream whose events are the result of the combination using the specified
	 * function of the input stream's event value and the value of the cell at that time.
     * <P>
     * There is an implicit delay: State updates caused by event firings being held with
     * {@link Stream#hold(Object)} don't become visible as the cell's current value until
     * the following transaction. To put this another way, {@link Stream#snapshot(Cell, Lambda2)}
     * always sees the value of a cell as it was before any state changes from the current
     * transaction.
     */
	public final <B,C> Stream<C> snapshot(final Cell<B> c, final Lambda2<A,B,C> f)
	{
	    final Stream<A> ev = this;
		final StreamWithSend<C> out = new StreamWithSend<C>();
        Listener l = listen_(out.node, new TransactionHandler<A>() {
        	public void run(Transaction trans2, A a) {
	            out.send(trans2, f.apply(a, c.sampleNoTrans()));
	        }
        });
        return out.unsafeAddCleanup(l);
	}

	/**
	 * Variant of {@link #snapshot(Cell, Lambda2)} that captures the values of
	 * two cells.
     */
	public final <B,C,D> Stream<D> snapshot(final Cell<B> cb, final Cell<C> cc, final Lambda3<A,B,C,D> fn)
	{
		return this.snapshot(cb, new Lambda2<A,B,D>() {
	    	public D apply(A a, B b) {
	    		return fn.apply(a, b, cc.sample());
	    	}
		});
	}

	/**
	 * Variant of {@link #snapshot(Cell, Lambda2)} that captures the values of
	 * three cells.
     */
	public final <B,C,D,E> Stream<E> snapshot(final Cell<B> cb, final Cell<C> cc, final Cell<D> cd, final Lambda4<A,B,C,D,E> fn)
	{
		return this.snapshot(cb, new Lambda2<A,B,E>() {
	    	public E apply(A a, B b) {
	    		return fn.apply(a, b, cc.sample(), cd.sample());
	    	}
		});
	}

	/**
	 * Variant of {@link #snapshot(Cell, Lambda2)} that captures the values of
	 * four cells.
     */
	public final <B,C,D,E,F> Stream<F> snapshot(final Cell<B> cb, final Cell<C> cc, final Cell<D> cd, final Cell<E> ce, final Lambda5<A,B,C,D,E,F> fn)
	{
		return this.snapshot(cb, new Lambda2<A,B,F>() {
	    	public F apply(A a, B b) {
	    		return fn.apply(a, b, cc.sample(), cd.sample(), ce.sample());
	    	}
		});
	}

	/**
	 * Variant of {@link #snapshot(Cell, Lambda2)} that captures the values of
	 * five cells.
     */
	public final <B,C,D,E,F,G> Stream<G> snapshot(final Cell<B> cb, final Cell<C> cc, final Cell<D> cd, final Cell<E> ce, final Cell<F> cf, final Lambda6<A,B,C,D,E,F,G> fn)
	{
		return this.snapshot(cb, new Lambda2<A,B,G>() {
	    	public G apply(A a, B b) {
	    		return fn.apply(a, b, cc.sample(), cd.sample(), ce.sample(), cf.sample());
	    	}
		});
	}

    /**
     * Variant of {@link Stream#merge(Stream, Lambda2)} that merges two streams and will drop an event
     * in the simultaneous case.
     * <p>
     * In the case where two events are simultaneous (i.e. both
     * within the same transaction), the event from <em>this</em> will take precedence, and
     * the event from <em>s</em> will be dropped.
     * If you want to specify your own combining function, use {@link Stream#merge(Stream, Lambda2)}.
     * s1.orElse(s2) is equivalent to s1.merge(s2, (l, r) -&gt; l).
     * <p>
     * The name orElse() is used instead of merge() to make it really clear that care should
     * be taken, because events can be dropped.
     */
	public final Stream<A> orElse(final Stream<A> s)
	{
	    return merge(s, new Lambda2<A,A,A>() {
            public A apply(A left, A right) { return left; }
        });
	}

	private static <A> Stream<A> merge(final Stream<A> ea, final Stream<A> eb)
	{
	    final StreamWithSend<A> out = new StreamWithSend<A>();
        final Node left = new Node(0);
        final Node right = out.node;
        Node.Target[] node_target_ = new Node.Target[1];
        left.linkTo(null, right, node_target_);
        final Node.Target node_target = node_target_[0];
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
     * Merge two streams of the same type into one, so that events on either input appear
     * on the returned stream.
     * <p>
     * If the events are simultaneous (that is, one event from this and one from <em>s</em>
     * occurring in the same transaction), combine them into one using the specified combining function
     * so that the returned stream is guaranteed only ever to have one event per transaction.
     * The event from <em>this</em> will appear at the left input of the combining function, and
     * the event from <em>s</em> will appear at the right.
     * @param f Function to combine the values. It may construct FRP logic or use
     *    {@link Cell#sample()}. Apart from this the function must be <em>referentially transparent</em>.
     */
    public final Stream<A> merge(final Stream<A> s, final Lambda2<A,A,A> f)
    {
	    return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
	    	public Stream<A> apply(Transaction trans) {
                return Stream.<A>merge(Stream.this, s).coalesce(trans, f);
	    	}
	    });
    }

    /**
     * Variant of {@link #orElse(Stream)} that merges a collection of streams.
     */
    public static <A> Stream<A> orElse(Iterable<Stream<A>> ss) {
        return Stream.<A>merge(ss, new Lambda2<A,A,A>() {
            public A apply(A left, A right) { return left; }
        });
    }

    /**
     * Variant of {@link #merge(Stream,Lambda2)} that merges a collection of streams.
     */
    public static <A> Stream<A> merge(Iterable<Stream<A>> ss, final Lambda2<A,A,A> f) {
        Vector<Stream<A>> v = new Vector<Stream<A>>();
        for (Stream<A> s : ss)
            v.add(s);
        return merge(v, 0, v.size(), f);
    }

    private static <A> Stream<A> merge(Vector<Stream<A>> sas, int start, int end, final Lambda2<A,A,A> f) {
        int len = end - start;
        if (len == 0) return new Stream<A>(); else
        if (len == 1) return sas.get(start); else
        if (len == 2) return sas.get(start).merge(sas.get(start+1), f); else {
            int mid = (start + end) / 2;
            return Stream.<A>merge(sas, start, mid, f).merge(Stream.<A>merge(sas, mid, end, f), f);
        }
    }

	private final Stream<A> coalesce(Transaction trans1, final Lambda2<A,A,A> f)
	{
	    final Stream<A> ev = this;
	    final StreamWithSend<A> out = new StreamWithSend<A>();
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
     * Return a stream that only outputs events for which the predicate returns true.
     */
    public final Stream<A> filter(final Lambda1<A,Boolean> predicate)
    {
        final Stream<A> ev = this;
        final StreamWithSend<A> out = new StreamWithSend<A>();
        Listener l = listen_(out.node, new TransactionHandler<A>() {
        	public void run(Transaction trans2, A a) {
	            if (predicate.apply(a)) out.send(trans2, a);
	        }
        });
        return out.unsafeAddCleanup(l);
    }

    /**
     * Return a stream that only outputs events that have present
     * values, removing the {@link java.util.Optional} wrapper, discarding empty values.
     */
    public static <A> Stream<A> filterOptional(final Stream<Optional<A>> ev)
    {
        final StreamWithSend<A> out = new StreamWithSend<A>();
        Listener l = ev.listen_(out.node, new TransactionHandler<Optional<A>>() {
        	public void run(Transaction trans2, Optional<A> oa) {
	            if (oa.isPresent()) out.send(trans2, oa.get());
	        }
        });
        return out.unsafeAddCleanup(l);
    }

    /**
     * Return a stream that only outputs events from the input stream
     * when the specified cell's value is true.
     */
    public final Stream<A> gate(Cell<Boolean> c)
    {
        return Stream.filterOptional(
            snapshot(c, new Lambda2<A,Boolean,Optional<A>>() {
                public Optional<A> apply(A a, Boolean pred) { return pred ? Optional.of(a) : Optional.<A>empty(); }
            })
        );
    }

    /**
     * Transform an event with a generalized state loop (a Mealy machine). The function
     * is passed the input and the old state and returns the new state and output value.
     * @param f Function to apply to update the state. It may construct FRP logic or use
     *    {@link Cell#sample()} in which case it is equivalent to {@link Stream#snapshot(Cell)}ing the
     *    cell. Apart from this the function must be <em>referentially transparent</em>.
     */
    public final <B,S> Stream<B> collect(final S initState, final Lambda2<A, S, Tuple2<B, S>> f)
    {
        return collectLazy(new Lazy<S>(initState), f);
    }

    /**
     * A variant of {@link #collect(Object, Lambda2)} that takes an initial state returned by
     * {@link Cell#sampleLazy()}.
     */
    public final <B,S> Stream<B> collectLazy(final Lazy<S> initState, final Lambda2<A, S, Tuple2<B, S>> f)
    {
        return Transaction.<Stream<B>>run(new Lambda0<Stream<B>>() {
            public Stream<B> apply() {
                final Stream<A> ea = Stream.this;
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
            }
        });
    }

    /**
     * Accumulate on input event, outputting the new state each time.
     * @param f Function to apply to update the state. It may construct FRP logic or use
     *    {@link Cell#sample()} in which case it is equivalent to {@link Stream#snapshot(Cell)}ing the
     *    cell. Apart from this the function must be <em>referentially transparent</em>.
     */
    public final <S> Cell<S> accum(final S initState, final Lambda2<A, S, S> f)
    {
        return accumLazy(new Lazy<S>(initState), f);
    }

    /**
     * A variant of {@link #accum(Object, Lambda2)} that takes an initial state returned by
     * {@link Cell#sampleLazy()}.
     */
    public final <S> Cell<S> accumLazy(final Lazy<S> initState, final Lambda2<A, S, S> f)
    {
        return Transaction.<Cell<S>>run(new Lambda0<Cell<S>>() {
            public Cell<S> apply() {
                final Stream<A> ea = Stream.this;
                StreamLoop<S> es = new StreamLoop<S>();
                Cell<S> s = es.holdLazy(initState);
                Stream<S> es_out = ea.snapshot(s, f);
                es.loop(es_out);
                return es_out.holdLazy(initState);
            }
        });
    }

    /**
     * Return a stream that outputs only one value: the next event of the
     * input stream, starting from the transaction in which once() was invoked.
     */
    public final Stream<A> once()
    {
        // This is a bit long-winded but it's efficient because it deregisters
        // the listener.
        final Stream<A> ev = this;
        final Listener[] la = new Listener[1];
        final StreamWithSend<A> out = new StreamWithSend<A>();
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

    /**
     * This is not thread-safe, so one of these two conditions must apply:
     * 1. We are within a transaction, since in the current implementation
     *    a transaction locks out all other threads.
     * 2. The object on which this is being called was created has not yet
     *    been returned from the method where it was created, so it can't
     *    be shared between threads.
     */
    Stream<A> unsafeAddCleanup(Listener cleanup)
    {
        finalizers.add(cleanup);
        return this;
    }

    /**
     * Attach a listener to this stream so that its {@link Listener#unlisten()} is invoked
     * when this stream is garbage collected. Useful for functions that initiate I/O,
     * returning the result of it through a stream.
     * <P>
     * You must use this only with listeners returned by {@link #listenWeak(Handler)} so that
     * things don't get kept alive when they shouldn't.
     */
    public Stream<A> addCleanup(final Listener cleanup) {
        return Transaction.run(new Lambda0<Stream<A>>() {
            public Stream<A> apply() {
                List<Listener> fsNew = new ArrayList<Listener>(finalizers);
                fsNew.add(cleanup);
                return new Stream<A>(node, fsNew, firings);
            }
        });
    }

	@Override
	protected void finalize() throws Throwable {
		for (Listener l : finalizers)
			l.unlisten();
	}
}
