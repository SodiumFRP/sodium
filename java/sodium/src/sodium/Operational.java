package sodium;

public class Operational {
    /**
     * A stream that gives the updates for the cell.
     *
     * This is an OPERATIONAL primitive, which is not part of the main Sodium
     * API. It breaks the property of non-detectability of cell steps/updates.
     * The rule with this primitive is that you should only use it in functions
     * that do not allow the caller to detect the cell updates.
     */
    public static final <A> Stream<A> updates(Cell<A> c)
    {
        return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
        	public Stream<A> apply(Transaction trans) {
                return c.updates(trans);
        	}
        });
    }

    /**
     * A stream that is guaranteed to fire once when you listen to it, giving
     * the current value of the cell, and thereafter behaves like updates(),
     * firing for each update to the cell's value.
     *
     * This is an OPERATIONAL primitive, which is not part of the main Sodium
     * API. It breaks the property of non-detectability of cell steps/updates.
     * The rule with this primitive is that you should only use it in functions
     * that do not allow the caller to detect the cell updates.
     */
    public static final <A> Stream<A> value(Cell<A> c)
    {
        return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
        	public Stream<A> apply(Transaction trans) {
        		return c.value(trans);
        	}
        });
    }
}
