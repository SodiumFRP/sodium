package nz.sodium;

/**
 * A stream that allows values to be pushed into it, acting as an interface between the
 * world of I/O and the world of FRP. Code that exports StreamSinks for read-only use
 * should downcast to {@link Stream}.
 */
public class StreamSink<A> extends StreamWithSend<A> {
    /**
     * Construct a StreamSink that allows send() to be called once on it per transaction.
     * If you call send() more than once, it will throw an exception. If you need to do
     * this, then use {@link StreamSink(Lambda2)}.
     */
    public StreamSink() {
        this(new Lambda2<A,A,A>() {
             public A apply(A left, A right) {
                 throw new RuntimeException("send() called more than once per transaction, which isn't allowed. Did you want to combine the events? Then pass a combining function to your StreamSink constructor.");
             }
         });
    }
    /**
     * If you send more than one event in a transaction, they are combined into a
     * single event using the specified function. The combining function should be
     * <em>associative</em>.
     * @param f Function to combine the values. It may construct FRP logic or use
     *    {@link Cell#sample()}. Apart from this the function must be <em>referentially transparent</em>.
     */
    public StreamSink(Lambda2<A,A,A> f) {
        this.coalescer = new CoalesceHandler<A>(f, this);
    }

    private CoalesceHandler<A> coalescer;

    /**
     * Send a value to be made available to consumers of the stream. send(A) may not be used inside
     * handlers registered with {@link Stream#listen(Handler)} or {@link Cell#listen(Handler)}.
     * An exception will be thrown, because StreamSink is for interfacing I/O to FRP only.
     * You are not meant to use this to define your own primitives.
     * @param a Value to push into the cell.
     */
	public void send(final A a) {
		Transaction.run(new Handler<Transaction>() {
			public void run(Transaction trans) {
                if (trans.inCallback > 0)
                    throw new RuntimeException("You are not allowed to use send() inside a Sodium callback");
                coalescer.run(trans, a);
            }
		});
	}
}
