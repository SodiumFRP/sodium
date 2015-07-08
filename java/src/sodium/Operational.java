package sodium;

/**
 * Operational primitives that must be used with care because they
 * break non-detectability of cell steps/updates.
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
    public static final <A> Stream<A> updates(final Cell<A> c)
    {
        return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
        	public Stream<A> apply(Transaction trans) {
                return c.updates(trans);
        	}
        });
    }

    /**
     * A stream that is guaranteed to fire once when you listen to it, giving
     * the current value of the cell, and thereafter behaves like {@link updates(Cell)},
     * firing for each update/step of the cell's value.
     * <P>
     * This is an OPERATIONAL primitive, which is not part of the main Sodium
     * API. It breaks the property of non-detectability of cell steps/updates.
     * The rule with this primitive is that you should only use it in functions
     * that do not allow the caller to detect the cell updates.
     */
    public static final <A> Stream<A> value(final Cell<A> c)
    {
        return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
        	public Stream<A> apply(Transaction trans) {
        		return c.value(trans);
        	}
        });
    }
}
