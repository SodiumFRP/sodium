package fridgets;

import sodium.*;
import java.awt.Dimension;

public class FridgetOutput<A> {
    public FridgetOutput(
        A out,
        Cell<Drawable> drawable,
        Cell<Dimension> desiredSize
    ) {
        this.out = out;
        this.drawable = drawable;
        this.desiredSize = desiredSize;
    }
    public A out;
    public Cell<Drawable> drawable;
    public Cell<Dimension> desiredSize;
}

