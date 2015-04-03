package sodium;

public final class CellSink<A> extends Cell<A> {
    public CellSink(A initValue) {
    	super(new StreamSink<A>(), initValue);
    }
    
    public void send(A a)
    {
        ((StreamSink<A>)str).send(a);
    }
}
