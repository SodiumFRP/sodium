import sodium.*;
import sodium.time.*;

public interface Animation {
    public Cell<Drawable> create(TimerSystem<Double> sys, Point extents);
}

