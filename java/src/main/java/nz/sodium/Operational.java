package nz.sodium;

import java.util.LinkedList;


/**
 * Operational primitives that must be used with care.
 */
public class Operational {
    /**
     * A stream that gives the updates/steps for a {@link Cell}.
     * <P>
     * This is an OPERATIONAL primitive, which is not part of the main Sodium
     * API. It breaks the property of non-detectability of cell steps/updates.
     * The rule with this primitive is that you should only use it in functions
     * that do not allow the caller to detect the cell updates.
     */
    public static <A> Stream<A> updates(final Cell<A> c)
    {
        return c.updates();
    }

    /**
     * A stream that is guaranteed to fire once in the transaction where value() is invoked, giving
     * the current value of the cell, and thereafter behaves like {@link #updates(Cell)},
     * firing for each update/step of the cell's value.
     * <P>
     * This is an OPERATIONAL primitive, which is not part of the main Sodium
     * API. It breaks the property of non-detectability of cell steps/updates.
     * The rule with this primitive is that you should only use it in functions
     * that do not allow the caller to detect the cell updates.
     */
    public static <A> Stream<A> value(final Cell<A> c)
    {
        return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
        	public Stream<A> apply(Transaction trans) {
        		return c.value(trans);
        	}
        });
    }

	/**
	 * Push each event onto a new transaction guaranteed to come before the next externally
	 * initiated transaction. Same as {@link #split(Stream)} but it works on a single value.
	 */
	public static <A> Stream<A> defer(Stream<A> s)
	{
	    return split(s.map(new Lambda1<A,Iterable<A>>() {
	        public Iterable<A> apply(A a) {
                LinkedList<A> l = new LinkedList<A>();
                l.add(a);
                return l;
            }
        }));
	}

	/**
	 * Push each event in the list onto a newly created transaction guaranteed
	 * to come before the next externally initiated transaction. Note that the semantics
	 * are such that two different invocations of split() can put events into the same
	 * new transaction, so the resulting stream's events could be simultaneous with
	 * events output by split() or {@link #defer(Stream)} invoked elsewhere in the code.
	 */
    public static <A, C extends Iterable<A>> Stream<A> split(Stream<C> s) {
	    final StreamWithSend<A> out = new StreamWithSend<A>();
	    Listener l1 = s.listen_(out.node, new TransactionHandler<C>() {
	        public void run(Transaction trans, C as) {
	            int childIx = 0;
                for (final A a : as) {
                    trans.post_(childIx, new Handler<Transaction>() {
                        public void run(Transaction trans) {
                            out.send(trans, a);
                        }
                    });
                    childIx++;
                }
	        }
	    });
	    return out.unsafeAddCleanup(l1);
    }
}

