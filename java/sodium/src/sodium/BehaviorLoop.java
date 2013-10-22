package sodium;

public final class BehaviorLoop<A> extends Behavior<A> {
    public BehaviorLoop() {
    	super(new EventLoop<A>(), null);
    }

    public void loop(Behavior<A> a_out)
    {
        ((EventLoop<A>)event).loop(a_out.updates());
        value = a_out.sample();
    }
}

