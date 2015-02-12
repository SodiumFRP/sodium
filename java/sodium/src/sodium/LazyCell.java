package sodium;

class LazyCell<A> extends Cell<A> {
    LazyCell(final Stream<A> event, final Lambda0<A> lazyInitValue) {
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

