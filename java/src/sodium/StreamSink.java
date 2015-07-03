package sodium;

/**
 * A stream that allows values to be pushed into it, acting as an interface between the
 * world of I/O and the world of FRP. Code that exports StreamSinks for read-only use
 * should downcast to {@link Stream}.
 */
public class StreamSink<A> extends StreamWithSend<A> {
    public StreamSink() {}

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
                send(trans, a);
            }
		});
	}
}
