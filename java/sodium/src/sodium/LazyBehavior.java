package sodium;

class LazyBehavior<A> extends Behavior<A> {
    LazyBehavior(final Event<A> event, final Lambda0<A> lazyInitValue) {
        super(event, null);
        this.lazyInitValue = lazyInitValue;
    }

    @Override
    protected A sampleNoTrans()
    {
        if (value == null) {
            value = lazyInitValue.apply();
            lazyInitValue = null;
        }
        return value;
    }
}

