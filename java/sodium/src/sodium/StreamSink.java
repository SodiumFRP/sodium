package sodium;

public class StreamSink<A> extends StreamWithSend<A> {
    public StreamSink() {}

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
