package sodium;

public class Behavior<A> {
	protected Event<A> event;
	A value;
	A valueUpdate;
	private Listener cleanup;

	/**
	 * A behavior with a constant value.
	 */
    public Behavior(A value)
    {
    	this.event = new Event<A>();
    	this.value = value;
    }

    Behavior(final Event<A> event, A initValue)
    {
    	this.event = event;
    	this.value = initValue;
    	Transaction.run(new Handler<Transaction>() {
    		public void run(Transaction trans1) {
	    		Behavior.this.cleanup = event.listen(Node.NULL, trans1, new TransactionHandler<A>() {
	    			public void run(Transaction trans2, A a) {
			    		if (Behavior.this.valueUpdate == null) {
			    			trans2.last(new Runnable() {
			    				public void run() {
				    				Behavior.this.value = Behavior.this.valueUpdate;
				    				Behavior.this.valueUpdate = null;
				    			}
			    			});
			    		}
			    		Behavior.this.valueUpdate = a;
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
    	return valueUpdate == null ? value :  valueUpdate;
    }

    /**
     * An event that gives the updates for the behavior. It doesn't do any equality
     * comparison as the name might imply.
     */
    public final Event<A> changes()
    {
    	return event;
    }

    /**
     * An event that is guaranteed to fire once when you listen to it, giving
     * the current value of the behavior, and thereafter behaves like 'changes',
     * firing for each update to the behavior's value.
     */
    public final Event<A> values()
    {
        return Transaction.evaluate(new Lambda1<Transaction, Event<A>>() {
        	public Event<A> evaluate(Transaction trans) {
        		return values(trans);
        	}
        });
    }

    final Event<A> values(Transaction trans1)
    {
    	final EventSink<A> out = new EventSink<A>() {
    		@Override
            protected Object[] sampleNow()
            {
                return new Object[] { value };
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
	public final <B> Behavior<B> map(Lambda1<A,B> f)
	{
		return changes().map(f).hold(f.evaluate(value));
	}

	/**
	 * Lift a binary function into behaviors.
	 */
	public final <B,C> Behavior<C> lift(final Lambda2<A,B,C> f, Behavior<B> b)
	{
		Lambda1<A, Lambda1<B,C>> ffa = new Lambda1<A, Lambda1<B,C>>() {
			public Lambda1<B,C> evaluate(final A aa) {
				return new Lambda1<B,C>() {
					public C evaluate(B bb) {
						return f.evaluate(aa,bb);
					}
				};
			}
		};
		Behavior<Lambda1<B,C>> bf = map(ffa);
		return apply(bf, b);
	}

	/**
	 * Lift a binary function into behaviors.
	 */
	public static final <A,B,C> Behavior<C> lift(Lambda2<A,B,C> f, Behavior<A> a, Behavior<B> b)
	{
		return a.lift(f, b);
	}

	/**
	 * Lift a ternary function into behaviors.
	 */
	public final <B,C,D> Behavior<D> lift(final Lambda3<A,B,C,D> f, Behavior<B> b, Behavior<C> c)
	{
		Lambda1<A, Lambda1<B, Lambda1<C,D>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C,D>>>() {
			public Lambda1<B, Lambda1<C,D>> evaluate(final A aa) {
				return new Lambda1<B, Lambda1<C,D>>() {
					public Lambda1<C,D> evaluate(final B bb) {
						return new Lambda1<C,D>() {
							public D evaluate(C cc) {
								return f.evaluate(aa,bb,cc);
							}
						};
					}
				};
			}
		};
		Behavior<Lambda1<B, Lambda1<C, D>>> bf = map(ffa);
		return apply(apply(bf, b), c);
	}

	/**
	 * Lift a ternary function into behaviors.
	 */
	public static final <A,B,C,D> Behavior<D> lift(Lambda3<A,B,C,D> f, Behavior<A> a, Behavior<B> b, Behavior<C> c)
	{
		return a.lift(f, b, c);
	}

	/**
	 * Apply a value inside a behavior to a function inside a behavior. This is the
	 * primitive for all function lifting.
	 */
	public static <A,B> Behavior<B> apply(final Behavior<Lambda1<A,B>> bf, final Behavior<A> ba)
	{
		final EventSink<B> out = new EventSink<B>();

        final Handler<Transaction> h = new Handler<Transaction>() {
            boolean fired = false;			
            @Override
            public void run(Transaction trans1) {
                if (fired) 
                    return;

                fired = true;
                trans1.prioritized(out.node, new Handler<Transaction>() {
                	public void run(Transaction trans2) {
                        out.send(trans2, bf.newValue().evaluate(ba.newValue()));
                        fired = false;
                    }
            	});
            }
        };

        Listener l1 = bf.changes().listen_(out.node, new TransactionHandler<Lambda1<A,B>>() {
        	public void run(Transaction trans1, Lambda1<A,B> f) {
                h.run(trans1);
            }
        });
        Listener l2 = ba.changes().listen_(out.node, new TransactionHandler<A>() {
        	public void run(Transaction trans1, A a) {
	            h.run(trans1);
	        }
        });
        return out.addCleanup(l1).addCleanup(l2).hold(bf.value.evaluate(ba.value));
	}

	/**
	 * Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
	 */
	public static <A> Behavior<A> switchB(final Behavior<Behavior<A>> bba)
	{
	    A za = bba.value.value;
	    final EventSink<A> out = new EventSink<A>();
        TransactionHandler<Behavior<A>> h = new TransactionHandler<Behavior<A>>() {
            private Listener currentListener;
            @Override
            public void run(Transaction trans2, Behavior<A> ba) {
                // Note: If any switch takes place during a transaction, then the
                // values().listen will always cause a sample to be fetched from the
                // one we just switched to. The caller will be fetching our output
                // using values().listen, and values() throws away all firings except
                // for the last one. Therefore, anything from the old input behaviour
                // that might have happened during this transaction will be suppressed.
                if (currentListener != null)
                    currentListener.unlisten();
                currentListener = ba.values(trans2).listen(out.node, trans2, new TransactionHandler<A>() {
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
        Listener l1 = bba.values().listen_(out.node, h);
        return out.addCleanup(l1).hold(za);
	}
	
	/**
	 * Unwrap an event inside a behavior to give a time-varying event implementation.
	 */
	public static <A> Event<A> switchE(final Behavior<Event<A>> bea)
	{
        return Transaction.evaluate(new Lambda1<Transaction, Event<A>>() {
        	public Event<A> evaluate(final Transaction trans) {
                return switchE(trans, bea);
        	}
        });
    }

	private static <A> Event<A> switchE(final Transaction trans1, final Behavior<Event<A>> bea)
	{
        final EventSink<A> out = new EventSink<A>();
        final TransactionHandler<A> h2 = new TransactionHandler<A>() {
        	public void run(Transaction trans2, A a) {
	            out.send(trans2, a);
	        }
        };
        TransactionHandler<Event<A>> h1 = new TransactionHandler<Event<A>>() {
            private Listener currentListener = bea.value.listen(out.node, trans1, h2, false);

            @Override
            public void run(final Transaction trans2, final Event<A> ea) {
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
        Listener l1 = bea.changes().listen(out.node, trans1, h1, false);
        return out.addCleanup(l1);
	}

	/**
	 * Loop a behavior round so it can effectively be forward referenced.
	 * This adds a cycle to your graph of relationships between events and behaviors.
	 */
    public static <A,B> B loop(final Lambda1<Behavior<A>,Tuple2<B,Behavior<A>>> f)
    {
        return Event.loop(
            // Lambda syntax doesn't work here - compiler bug?
            new Lambda1<Event<A>,Tuple2<B,Event<A>>>() {
                public Tuple2<B,Event<A>> evaluate(Event<A> ea) {
                    Tuple2<B,Behavior<A>> b_ba = f.evaluate(ea.hold(null));
                    return new Tuple2<B, Event<A>>(b_ba.a, b_ba.b.values());
                }
            }
        );
    }

    /**
     * Transform a behavior with a generalized state loop (a mealy machine). The function
     * is passed the input and the old state and returns the new state and output value.
     */
    public final <B,S> Behavior<B> collect(final S initState, final Lambda2<A, S, Tuple2<B, S>> f)
    {
        final Event<A> ea = changes().coalesce(new Lambda2<A,A,A>() {
        	public A evaluate(A fst, A snd) { return snd; }
        });
        final A za = value;
        final Tuple2<B, S> zbs = f.evaluate(za, initState);
        return Event.loop(
            new Lambda1<Event<Tuple2<B,S>>, Tuple2<Behavior<B>,Event<Tuple2<B,S>>>>() {
                public Tuple2<Behavior<B>,Event<Tuple2<B,S>>> evaluate(Event<Tuple2<B,S>> ebs) {
                    Behavior<Tuple2<B,S>> bbs = ebs.hold(zbs);
                    Behavior<S> bs = bbs.map(new Lambda1<Tuple2<B,S>,S>() {
                    	public S evaluate(Tuple2<B,S> x) {
                    	    return x.b;
                    	}
                    });
                    Event<Tuple2<B,S>> ebs_out = ea.snapshot(bs, f);
                    return new Tuple2<Behavior<B>,Event<Tuple2<B,S>>>(bbs.map(new Lambda1<Tuple2<B,S>,B>() {
                    	public B evaluate(Tuple2<B,S> x) {
                    	    return x.a;
                    	}
                    }), ebs_out);
                }
            }
        );
    }

    /**
     * Accumulate on input event, holding state.
     */
    public final <S> Behavior<S> accum(final S initState, final Lambda2<A, S, S> f)
    {
        final Event<A> ea = changes().coalesce(new Lambda2<A,A,A>() {
        	public A evaluate(A fst, A snd) { return snd; }
        });
        final A za = value;
        final S zs = f.evaluate(za, initState);
        return Event.loop(
            new Lambda1<Event<S>, Tuple2<Behavior<S>,Event<S>>>() {
                public Tuple2<Behavior<S>,Event<S>> evaluate(Event<S> es) {
                    Behavior<S> bs = es.hold(zs);
                    Event<S> es_out = ea.snapshot(bs, f);
                    return new Tuple2<Behavior<S>,Event<S>>(bs, es_out);
                }
            }
        );
    }

	@Override
	protected void finalize() throws Throwable {
	    if (cleanup != null)
            cleanup.unlisten();
	}
}
