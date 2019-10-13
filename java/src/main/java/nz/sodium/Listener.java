package nz.sodium;

/**
 * A handle for a listener that was registered with {@link Cell#listen(Handler)} or {@link Stream#listen(Handler)}.
 */
public class Listener {
    public Listener() {}

    /**
     * Deregister the listener that was registered so it will no longer be called back,
     * allowing associated resources to be garbage collected.
     */
    public void unlisten() {}

    /**
     * Combine listeners into one so that invoking {@link #unlisten()} on the returned
     * listener will unlisten both the inputs.
     */
    public final Listener append(final Listener two) {
        final Listener one = this;
        return new Listener() {
            public void unlisten() {
                one.unlisten();
                two.unlisten();
            }
        };
    }
}

