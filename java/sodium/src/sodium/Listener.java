package sodium;

import java.util.List;
import java.util.ArrayList;

public class Listener {
    public Listener() {}

    public void unlisten() {}

    /**
     * Combine listeners into one where a single unlisten() invocation will unlisten
     * both the inputs.
     */
    public Listener append(final Listener l) {
        return new Listener() {
            public void unlisten() {
                this.unlisten();
                l.unlisten();
            }
        };
    }
}

