package sodium;

public class Listener {
    public Listener() {}

    public void unlisten() {}

    /**
     * Combine listeners into one where a single unlisten() invocation will unlisten
     * both the inputs.
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

