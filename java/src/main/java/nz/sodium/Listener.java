package nz.sodium;

/**
 * A handle for a listener that was registered with {@link Cell#listen(Handler)} or {@link Stream#listen(Handler)}.
 * To keep the FRP logic alive, you must prevent this object being garbage
 * collected. You're recommended either to ensure you call {@link unlisten()} when you've
 * finished with it, or to store it in a field of an object that you know will stay in memory.
 */
public class Listener {
    public Listener() {}

    /**
     * Deregister the listener that was registered so it will no longer be called back,
     * allowing associated resources to be garbage collected.
     */
    public void unlisten() {}

    /**
     * Combine listeners into one so that invoking {@link unlisten()} on the returned
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

