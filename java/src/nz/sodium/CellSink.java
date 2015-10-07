package nz.sodium;

/**
 * A cell that allows values to be pushed into it, acting as an interface between the
 * world of I/O and the world of FRP. Code that exports CellSinks for read-only use
 * should downcast to {@link Cell}.
 */
public final class CellSink<A> extends Cell<A> {
    /**
     * Construct a writable cell with the specified initial value. If multiple values are
     * sent in the same transaction, the last one is used.
     */
    public CellSink(A initValue) {
    	super(new StreamSink<A>(), initValue);
    }

    /**
     * Construct a writable cell with the specified initial value. If multiple values are
     * sent in the same transaction, the specified function is used to combine them.
     */
    public CellSink(A initValue, Lambda2<A,A,A> f) {
    	super(new StreamSink<A>(f), initValue);
    }

    /**
     * Send a value, modifying the value of the cell. send(A) may not be used inside
     * handlers registered with {@link Stream#listen(Handler)} or {@link Cell#listen(Handler)}.
     * An exception will be thrown, because CellSink is for interfacing I/O to FRP only.
     * You are not meant to use this to define your own primitives.
     * @param a Value to push into the cell.
     */
    public void send(A a)
    {
        ((StreamSink<A>)str).send(a);
    }
}
