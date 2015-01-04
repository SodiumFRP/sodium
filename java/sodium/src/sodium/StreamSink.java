package sodium;

public class StreamSink<A> extends StreamWithSend<A> {
    public StreamSink() {}

	public void send(final A a) {
		Transaction.run(new Handler<Transaction>() {
			public void run(Transaction trans) { send(trans, a); }
		});
	}
}
