import sodium.*;
import java.util.Optional;

public class ValueOutput<A> {
    public ValueOutput(Cell<Optional<A>> output, Listener cleanup) {
        this.output = output;
        this.cleanup = cleanup;
    }
    public final Cell<Optional<A>> output;
    public final Listener cleanup;
}

