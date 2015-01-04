package sodium;

import java.util.List;

public class StreamWithSend<A> extends Stream<A> {

	protected void send(Transaction trans, A a) {
		if (firings.isEmpty())
			trans.last(new Runnable() {
				public void run() {
					firings.clear();
				}
			});
		firings.add(a);

		@SuppressWarnings("unchecked")
		List<TransactionHandler<A>> listeners = (List<TransactionHandler<A>>) this.listeners
				.clone();
		for (TransactionHandler<A> action : listeners) {
			try {
				action.run(trans, a);
			} catch (Throwable t) {
				t.printStackTrace();
			}
		}
	}

}
