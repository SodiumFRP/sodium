package sodium;

class LazyCell<A> extends Cell<A> {
    LazyCell(final Stream<A> event, final Lazy<A> lazyInitValue) {
        super(event, null);
        this.lazyInitValue = lazyInitValue;
    }

    @Override
    protected A sampleNoTrans()
    {
        if (value == null && lazyInitValue != null) {
            value = lazyInitValue.get();
            lazyInitValue = null;
        }
        return value;
    }
}

