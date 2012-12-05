package sodium;

public final class BehaviorSink<A> extends Behavior<A> {
    public BehaviorSink(A initValue) {
    	super(new EventSink<A>(), initValue);
    }
    
    public void send(A a)
    {
        ((EventSink<A>)event).send(a);
    }
}
