package fridgets;

import java.awt.Dimension;
import java.awt.event.MouseEvent;
import java.util.Optional;
import sodium.*;

public abstract class Fridget {
    public static class Output {
        public Output(
                Cell<Drawable> drawable,
                Cell<Dimension> desiredSize) {
            this.drawable = drawable;
            this.desiredSize = desiredSize;
        }
        public Cell<Drawable> drawable;
        public Cell<Dimension> desiredSize;
    }
    public Fridget(Lambda3<
            Cell<Optional<Dimension>>,
            Stream<MouseEvent>,
            Supply,
            Output> reify_) {
        this.reify_ = reify_;
    }
    private final Lambda3<
        Cell<Optional<Dimension>>,
        Stream<MouseEvent>,
        Supply,
        Output> reify_;
    public final Output reify(
            Cell<Optional<Dimension>> size,
            Stream<MouseEvent> sMouse,
            Supply idSupply) {
        return reify_.apply(size, sMouse, idSupply);
    }
}

