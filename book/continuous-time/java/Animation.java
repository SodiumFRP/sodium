import nz.sodium.*;
import nz.sodium.time.*;

public interface Animation {
    public Cell<Drawable> create(TimerSystem<Double> sys, Point extents);
}

