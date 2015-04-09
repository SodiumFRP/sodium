package fridgets;

import java.awt.Dimension;
import java.awt.event.MouseEvent;
import java.util.Optional;
import sodium.*;

public interface Fridget<A> {
    public abstract FridgetOutput<A> reify(
            Cell<Optional<Dimension>> size,
            Stream<MouseEvent> sMouse
        );
}

