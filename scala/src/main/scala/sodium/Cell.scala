package sodium

class Cell[A](protected val event: Stream[A], var value: Option[A]) {
	
	var valueUpdate: Option[A] = None 
	private var cleanup: Option[Listener] = None
    protected var lazyInitValue: Option[() => A] = None   // Used by LazyCell

	/**
	 * A behavior with a constant value.
	 */
    def this(value: Option[A]) {
	    this(new Stream[A](), value)
    }
	
	def this(value: A) {
	  this(new Stream[A](), Some(value))
	}

    // note before this was only called from the main constructor
    // not the secondary constructor

	Transaction.run({
	  trans1 =>
    		Cell.this.cleanup = Some(event.listen(Node.NULL, trans1, new TransactionHandler[A]() {
    			def run(trans2: Transaction, a: A) {
		    		if (Cell.this.valueUpdate.isEmpty) {
		    			trans2.last(new Runnable() {
		    				def run() {
			    				Cell.this.value = Cell.this.valueUpdate
			    				Cell.this.lazyInitValue = None
			    				Cell.this.valueUpdate = None
			    			}
		    			})
		    		}
		    		Cell.this.valueUpdate = Some(a)
		    	}
    		}, false))
		})

    /**
     * @return The value including any updates that have happened in this transaction.
     */
    def newValue(): A = valueUpdate.getOrElse(sampleNoTrans)

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
    def sample(): Option[A] = 
	    Transaction.apply(trans => sampleNoTrans())

    def sampleNoTrans(): Option[A] = value

    /**
     * An event that gives the updates for the behavior. If this behavior was created
     * with a hold, then updates() gives you an event equivalent to the one that was held.
     */
    final def updates(): Stream[A] = event

    /**
     * An event that is guaranteed to fire once when you listen to it, giving
     * the current value of the behavior, and thereafter behaves like updates(),
     * firing for each update to the behavior's value.
     */
    
    // TODO: I made this take an option .. not sure if this was the right choice ..
    
    final def value(trans1: Option[Transaction] = None): Stream[A] = 
        trans1 match {
    	case Some(trans1) =>
    	val out: StreamSink[A] = new StreamSink[A]() {
            protected override def sampleNow(): IndexedSeq[A] =
               IndexedSeq(sampleNoTrans())
    	}
        val l: Listener = event.listen(out.node, trans1,
    		new TransactionHandler[A]() {
	        	def run(trans2: Transaction, a: A ) { out.send(trans2, a) }
	        }, false)
	        // Needed in case of an initial value and an update in the same transaction.
        out.addCleanup(l)
            .lastFiringOnly(trans1)
    	case None =>
	        Transaction.apply(trans => value(Some(trans)))
    }

    /**
     * Transform the behavior's value according to the supplied function.
     */
	final def map[B](f: A => B): Cell[B] =
		updates().map(f).holdLazy(() => f.apply(sampleNoTrans()))

	/**
	 * Lift a binary function into behaviors.
	 */
	final def lift[B,C](f: (A, B) => C, b: Cell[B]): Cell[C] =
	{
		 def ffa: Lambda1[A, Lambda1[B,C]] = new Lambda1[A, Lambda1[B,C]]() {
			def apply(aa: A): Lambda1[B,C] = {
				new Lambda1[B,C]() {
					def apply(bb: B): C {
						f.apply(aa,bb)
					}
				}
			}
		}
		val bf: Cell[Lambda1[B,C]] = map(ffa)
		apply(bf, b)
	}

	/**
	 * Lift a ternary function into behaviors.
	 */
	 final <B,C,D> Cell<D> lift(final Lambda3<A,B,C,D> f, Cell<B> b, Cell<C> c)
	{
		Lambda1<A, Lambda1<B, Lambda1<C,D>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C,D>>>() {
			 Lambda1<B, Lambda1<C,D>> apply(final A aa) {
				return new Lambda1<B, Lambda1<C,D>>() {
					 Lambda1<C,D> apply(final B bb) {
						return new Lambda1<C,D>() {
							 D apply(C cc) {
								return f.apply(aa,bb,cc)
							}
						}
					}
				}
			}
		}
		Cell<Lambda1<B, Lambda1<C, D>>> bf = map(ffa)
		return apply(apply(bf, b), c)
	}

	/**
	 * Lift a quaternary function into behaviors.
	 */
	 final <B,C,D,E> Cell<E> lift(final Lambda4<A,B,C,D,E> f, Cell<B> b, Cell<C> c, Cell<D> d)
	{
		Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D,E>>>> ffa = new Lambda1<A, Lambda1<B, Lambda1<C, Lambda1<D,E>>>>() {
			 Lambda1<B, Lambda1<C, Lambda1<D,E>>> apply(final A aa) {
				return new Lambda1<B, Lambda1<C, Lambda1<D,E>>>() {
					 Lambda1<C, Lambda1<D, E>> apply(final B bb) {
						return new Lambda1<C, Lambda1<D,E>>() {
							 Lambda1<D,E> apply(final C cc) {
                                return new Lambda1<D, E>() {
                                     E apply(D dd) {
                                        return f.apply(aa,bb,cc,dd)
                                    }
                                }
							}
						}
					}
				}
			}
		}
		Cell<Lambda1<B, Lambda1<C, Lambda1<D, E>>>> bf = map(ffa)
		return apply(apply(apply(bf, b), c), d)
	}

    /**
     * Transform a behavior with a generalized state loop (a mealy machine). The function
     * is passed the input and the old state and returns the new state and output value.
     */
    final <B,S> Cell<B> collect(final S initState, final Lambda2<A, S, Tuple2<B, S>> f)
    {
        return Transaction.<Cell<B>>run(() -> {
            final Stream<A> ea = updates().coalesce(new Lambda2<A,A,A>() {
                 A apply(A fst, A snd) { return snd }
            })
            final Lambda0<Tuple2<B, S>> zbs = () -> f.apply(sampleNoTrans(), initState)
            StreamLoop<Tuple2<B,S>> ebs = new StreamLoop<Tuple2<B,S>>()
            Cell<Tuple2<B,S>> bbs = ebs.holdLazy(zbs)
            Cell<S> bs = bbs.map(new Lambda1<Tuple2<B,S>,S>() {
                 S apply(Tuple2<B,S> x) {
                    return x.b
                }
            })
            Stream<Tuple2<B,S>> ebs_out = ea.snapshot(bs, f)
            ebs.loop(ebs_out)
            return bbs.map(new Lambda1<Tuple2<B,S>,B>() {
                 B apply(Tuple2<B,S> x) {
                    return x.a
                }
            })
        })
    }

	protected override def finalize() {
	    if (cleanup.isDefined)
            cleanup.unlisten()
	}
}

object Cell {
	
	/**
	 * Lift a binary function into behaviors.
	 */
	final def lift[A,B,C](f: (A, B) => C, a: Cell[A], b: Cell[B]): Cell[C] = 
		a.lift(f, b)
	
	/**
	 * Lift a ternary function into behaviors.
	 */
	final def lift[A,B,C,D](f: (A, B, C) => D, a: Cell[A], b: Cell[B], c: Cell[C]): Cell[D] =
		a.lift(f, b, c)
		
   /**
	 * Lift a quaternary function into behaviors.
	 */
	final def lift[A,B,C,D,E](f: (A, B, C, D) => E, a: Cell[A], b: Cell[B], c: Cell[C], d: Cell[D]): Cell[E] = 
		a.lift(f, b, c, d)
	
			/**
	 * Apply a value inside a behavior to a function inside a behavior. This is the
	 * primitive for all function lifting.
	 */
	def apply[A,B](bf: Cell[A => B], ba: Cell[A]): Cell[B] = {
		val out = new StreamSink[B]()

		var fired = false
        def h(trans1: Transaction) {
                if (fired) 
                    return

                fired = true
                trans1.prioritized(out.node, {
                  trans2 =>
                        out.send(trans2, bf.newValue().apply(ba.newValue()))
                        fired = false
            	})
        }

        val l1 = bf.updates().listen_(out.node, new TransactionHandler[A => B]() {
        	def run(trans1: Transaction, f: A => B) {
                h(trans1)
            }
        })
        val l2 = ba.updates().listen_(out.node, new TransactionHandler[A]() {
        	def run(trans1: Transaction, a: A) {
	            h(trans1)
	        }
        })
        return out.addCleanup(l1).addCleanup(l2).holdLazy(() -> bf.sampleNoTrans().apply(ba.sampleNoTrans()))
	}
	
	/**
	 * Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
	 */
	def switchC[A](bba: Cell[Cell[A]]): Cell[A] =
	{
	    def za: A = bba.sampleNoTrans().sampleNoTrans()
	    val out = new StreamSink[A]()
        val h = new TransactionHandler[Cell[A]]() {
            private var currentListener: Listener = _
               override def run(trans2: Transaction, ba: Cell[A]) {
                // Note: If any switch takes place during a transaction, then the
                // value().listen will always cause a sample to be fetched from the
                // one we just switched to. The caller will be fetching our output
                // using value().listen, and value() throws away all firings except
                // for the last one. Therefore, anything from the old input behaviour
                // that might have happened during this transaction will be suppressed.
                if (currentListener != null)
                    currentListener.unlisten()
                currentListener = ba.value(Some(trans2)).listen(out.node, trans2, new TransactionHandler[A]() {
                	def run(trans3: Transaction, a: A) {
	                    out.send(trans3, a)
	                }
                }, false)
            }

            override def finalize() {
                if (currentListener != null)
                    currentListener.unlisten()
            }
        }
        val l1 = bba.value().listen_(out.node, h)
        return out.addCleanup(l1).holdLazy(za)
	}
	
	/**
	 * Unwrap an event inside a behavior to give a time-varying event implementation.
	 */
	def switchS[A](bea: Cell[Stream[A]]): Stream[A] = 
	{
        return Transaction.apply(new Lambda1[Transaction, Stream[A]]() {
        	def apply(trans: Transaction): Stream[A] = 
                switchS(trans, bea)
        })
    }

	private def switchS[A](trans1: Transaction, bea: Cell[Stream[A]]): Stream[A] = 
	{
        val out = new StreamSink[A]()
        val h2 = new TransactionHandler[A]() {
        	def run(trans2: Transaction, a: A) {
	            out.send(trans2, a)
	        }
        }
        val h1 = new TransactionHandler[Stream[A]]() {
            private var currentListener = bea.sampleNoTrans().listen(out.node, trans1, h2, false)

            override def run(trans2: Transaction, ea: Stream[A]) {
                trans2.last(new Runnable() {
                	def run() {
	                    if (currentListener != null)
	                        currentListener.unlisten()
	                    currentListener = ea.listen(out.node, trans2, h2, true)
	                }
                })
            }

            override def finalize() {
                if (currentListener != null)
                    currentListener.unlisten()
            }
        }
        val l1 = bea.updates().listen(out.node, trans1, h1, false)
        return out.addCleanup(l1)
	}
}
