package sodium;

public class Cell<A> {
	protected Stream<A> event;
	A value;
	A valueUpdate;
	private Listener cleanup;
    protected Lambda0<A> lazyInitValue;  // Used by LazyCell

	/**
	 * A behavior with a constant value.
	 */
    public Cell(A value)
    {
    	this.event = new Stream<A>();
    	this.value = value;
    }

    Cell(final Stream<A> event, A initValue)
    {
    	this.event = event;
    	this.value = initValue;
    	Transaction.run(new Handler<Transaction>() {
    		public void run(Transaction trans1) {
	    		Cell.this.cleanup = event.listen(Node.NULL, trans1, new TransactionHandler<A>() {
	    			public void run(Transaction trans2, A a) {
			    		if (Cell.this.valueUpdate == null) {
			    			trans2.last(new Runnable() {
			    				public void run() {
				    				Cell.this.value = Cell.this.valueUpdate;
				    				Cell.this.lazyInitValue = null;
				    				Cell.this.valueUpdate = null;
				    			}
			    			});
			    		}
			    		Cell.this.valueUpdate = a;
			    	}
	    		}, false);
    		}
    	});
    }

    /**
     * @return The value including any updates that have happened in this transaction.
     */
    final A newValue()
    {
    	return valueUpdate == null ? sampleNoTrans() :  valueUpdate;
    }

    /**
     * Sample the behavior's current value.
     *
     * This should generally be avoided in favour of value().listen(..) so you don't
     * miss any updates, but in many circumstances it makes sense.
     *
     * It can be best to use it inside an explicit transaction (using Transaction.run()).
     * For example, a b.sample() inside an explicit transaction along with a
     * b.updates().listen(..) will capture the current value and any updates without risk
     * of missing any in between.
     */
    public final A sample()
    {
        return Transaction.apply(new Lambda1<Transaction, A>() {
        	public A apply(Transaction trans) {
        		return sampleNoTrans();
        	}
        });
    }

    protected A sampleNoTrans()
    {
        return value;
    }

    /**
     * An event that gives the updates for the behavior. If this behavior was created
     * with a hold, then updates() gives you an event equivalent to the one that was held.
     */
    public final Stream<A> updates()
    {
    	return event;
    }

    /**
     * An event that is guaranteed to fire once when you listen to it, giving
     * the current value of the behavior, and thereafter behaves like updates(),
     * firing for each update to the behavior's value.
     */
    public final Stream<A> value()
    {
        return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
        	public Stream<A> apply(Transaction trans) {
        		return value(trans);
        	}
        });
    }

    final Stream<A> value(Transaction trans1)
    {
    	final StreamSink<A> out = new StreamSink<A>() {
    		@Override
            protected Object[] sampleNow()
            {
                return new Object[] { sampleNoTrans() };
            }
    	};
        Listener l = event.listen(out.node, trans1,
    		new TransactionHandler<A>() {
	        	public void run(Transaction trans2, A a) { out.send(trans2, a); }
	        }, false);
        return out.addCleanup(l)
            .lastFiringOnly(trans1);  // Needed in case of an initial value and an update
    	                              // in the same transaction.
    }

    /**
     * Transform the behavior's value according to the supplied function.
     */
	public final <B> Cell<B> map(Lambda1<A,B> f)
	{
		return updates().map(f).holdLazy(() -> f.apply(sampleNoTrans()));
	}

	/**
	 * Lift a binary function into behaviors.
	 */
	public final <B,C> Cell<C> lift(final Lambda2<A,B,C> f, Cell<B> b)
	{
		Lambda1<A, Lambda1<B,C>> ffa = new Lambda1<A, Lambda1<B,C>>() {
			public Lambda1<B,C> apply(final A aa) {
				return new Lambda1<B,C>() {
					public C apply(B bb) {
						return f.apply(aa,bb);
					}
				};
			}
		};
		Cell<Lambda1<B,C>> bf = map(ffa);
		return apply(bf, b);
	}

	/**
	 * Lift a binary function into behaviors.
	 */
	public static final <A,B,C> Cell<C> lift(Lambda2<A,B,C> f, Cell<A> a, Cell<B> b)
	{
		return a.lift(f, b);
	}

	/**
	 * Lift a ternary function into behaviors.
	 */
	public final <B,C,D> Cell<D> lift(final Lambda3<A,B,C,D> f, Cell<B> b, Cell<C> c)
	{
		Lambda1<A, Lambda1<B, Lambda1<C,D>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C,D>>>() {
			public Lambda1<B, Lambda1<C,D>> apply(final A aa) {
				return new Lambda1<B, Lambda1<C,D>>() {
					public Lambda1<C,D> apply(final B bb) {
						return new Lambda1<C,D>() {
							public D apply(C cc) {
								return f.apply(aa,bb,cc);
							}
						};
					}
				};
			}
		};
		Cell<Lambda1<B, Lambda1<C, D>>> bf = map(ffa);
		return apply(apply(bf, b), c);
	}

	/**
	 * Lift a ternary function into behaviors.
	 */
	public static final <A,B,C,D> Cell<D> lift(Lambda3<A,B,C,D> f, Cell<A> a, Cell<B> b, Cell<C> c)
	{
		return a.lift(f, b, c);
	}

	/**
	 * Lift a quaternary function into behaviors.
	 */
	public final <B,C,D,E> Cell<E> lift(final Lambda4<A,B,C,D,E> f, Cell<B> b, Cell<C> c, Cell<D> d)
	{
		Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D,E>>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D,E>>>>() {
			public Lambda1<B, Lambda1<C, Lambda1<D,E>>> apply(final A aa) {
				return new Lambda1<B, Lambda1<C, Lambda1<D,E>>>() {
					public Lambda1<C, Lambda1<D, E>> apply(final B bb) {
						return new Lambda1<C, Lambda1<D,E>>() {
							public Lambda1<D,E> apply(final C cc) {
                                return new Lambda1<D, E>() {
                                    public E apply(D dd) {
                                        return f.apply(aa,bb,cc,dd);
                                    }
                                };
							}
						};
					}
				};
			}
		};
		Cell<Lambda1<B, Lambda1<C, Lambda1<D, E>>>> bf = map(ffa);
		return apply(apply(apply(bf, b), c), d);
	}

	/**
	 * Lift a quaternary function into behaviors.
	 */
	public static final <A,B,C,D,E> Cell<E> lift(Lambda4<A,B,C,D,E> f, Cell<A> a, Cell<B> b, Cell<C> c, Cell<D> d)
	{
		return a.lift(f, b, c, d);
	}

	/**
	 * Apply a value inside a behavior to a function inside a behavior. This is the
	 * primitive for all function lifting.
	 */
	public static <A,B> Cell<B> apply(final Cell<Lambda1<A,B>> bf, final Cell<A> ba)
	{
		final StreamSink<B> out = new StreamSink<B>();

        final Handler<Transaction> h = new Handler<Transaction>() {
            boolean fired = false;			
            @Override
            public void run(Transaction trans1) {
                if (fired) 
                    return;

                fired = true;
                trans1.prioritized(out.node, new Handler<Transaction>() {
                	public void run(Transaction trans2) {
                        out.send(trans2, bf.newValue().apply(ba.newValue()));
                        fired = false;
                    }
            	});
            }
        };

        Listener l1 = bf.updates().listen_(out.node, new TransactionHandler<Lambda1<A,B>>() {
        	public void run(Transaction trans1, Lambda1<A,B> f) {
                h.run(trans1);
            }
        });
        Listener l2 = ba.updates().listen_(out.node, new TransactionHandler<A>() {
        	public void run(Transaction trans1, A a) {
	            h.run(trans1);
	        }
        });
        return out.addCleanup(l1).addCleanup(l2).holdLazy(() -> bf.sampleNoTrans().apply(ba.sampleNoTrans()));
	}

	/**
	 * Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
	 */
	public static <A> Cell<A> switchC(final Cell<Cell<A>> bba)
	{
	    Lambda0<A> za = () -> bba.sampleNoTrans().sampleNoTrans();
	    final StreamSink<A> out = new StreamSink<A>();
        TransactionHandler<Cell<A>> h = new TransactionHandler<Cell<A>>() {
            private Listener currentListener;
            @Override
            public void run(Transaction trans2, Cell<A> ba) {
                // Note: If any switch takes place during a transaction, then the
                // value().listen will always cause a sample to be fetched from the
                // one we just switched to. The caller will be fetching our output
                // using value().listen, and value() throws away all firings except
                // for the last one. Therefore, anything from the old input behaviour
                // that might have happened during this transaction will be suppressed.
                if (currentListener != null)
                    currentListener.unlisten();
                currentListener = ba.value(trans2).listen(out.node, trans2, new TransactionHandler<A>() {
                	public void run(Transaction trans3, A a) {
	                    out.send(trans3, a);
	                }
                }, false);
            }

            @Override
            protected void finalize() throws Throwable {
                if (currentListener != null)
                    currentListener.unlisten();
            }
        };
        Listener l1 = bba.value().listen_(out.node, h);
        return out.addCleanup(l1).holdLazy(za);
	}
	
	/**
	 * Unwrap an event inside a behavior to give a time-varying event implementation.
	 */
	public static <A> Stream<A> switchS(final Cell<Stream<A>> bea)
	{
        return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
        	public Stream<A> apply(final Transaction trans) {
                return switchS(trans, bea);
        	}
        });
    }

	private static <A> Stream<A> switchS(final Transaction trans1, final Cell<Stream<A>> bea)
	{
        final StreamSink<A> out = new StreamSink<A>();
        final TransactionHandler<A> h2 = new TransactionHandler<A>() {
        	public void run(Transaction trans2, A a) {
	            out.send(trans2, a);
	        }
        };
        TransactionHandler<Stream<A>> h1 = new TransactionHandler<Stream<A>>() {
            private Listener currentListener = bea.sampleNoTrans().listen(out.node, trans1, h2, false);

            @Override
            public void run(final Transaction trans2, final Stream<A> ea) {
                trans2.last(new Runnable() {
                	public void run() {
	                    if (currentListener != null)
	                        currentListener.unlisten();
	                    currentListener = ea.listen(out.node, trans2, h2, true);
	                }
                });
            }

            @Override
            protected void finalize() throws Throwable {
                if (currentListener != null)
                    currentListener.unlisten();
            }
        };
        Listener l1 = bea.updates().listen(out.node, trans1, h1, false);
        return out.addCleanup(l1);
	}

    /**
     * Transform a behavior with a generalized state loop (a mealy machine). The function
     * is passed the input and the old state and returns the new state and output value.
     */
    public final <B,S> Cell<B> collect(final S initState, final Lambda2<A, S, Tuple2<B, S>> f)
    {
        return Transaction.<Cell<B>>run(() -> {
            final Stream<A> ea = updates().coalesce(new Lambda2<A,A,A>() {
                public A apply(A fst, A snd) { return snd; }
            });
            final Lambda0<Tuple2<B, S>> zbs = () -> f.apply(sampleNoTrans(), initState);
            StreamLoop<Tuple2<B,S>> ebs = new StreamLoop<Tuple2<B,S>>();
            Cell<Tuple2<B,S>> bbs = ebs.holdLazy(zbs);
            Cell<S> bs = bbs.map(new Lambda1<Tuple2<B,S>,S>() {
                public S apply(Tuple2<B,S> x) {
                    return x.b;
                }
            });
            Stream<Tuple2<B,S>> ebs_out = ea.snapshot(bs, f);
            ebs.loop(ebs_out);
            return bbs.map(new Lambda1<Tuple2<B,S>,B>() {
                public B apply(Tuple2<B,S> x) {
                    return x.a;
                }
            });
        });
    }

	@Override
	protected void finalize() throws Throwable {
	    if (cleanup != null)
            cleanup.unlisten();
	}
}
