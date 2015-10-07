import nz.sodium.*;
import java.util.Optional;

public class ValueOutput<A> {
    public ValueOutput(Cell<Optional<A>> value, Listener cleanup) {
        this.value = value;
        this.cleanup = cleanup;
    }
    public final Cell<Optional<A>> value;
    public final Listener cleanup;
}

