package sodium;

public interface TransactionHandler<A> {
    void run(Transaction trans, A a);
}

