package sodium;

interface TransactionHandler<A> {
    void run(Transaction trans, A a);
}

