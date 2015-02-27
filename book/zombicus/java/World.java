import java.awt.Dimension;
import java.awt.Point;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.List;

public class World {
    public World(Dimension windowSize) {
        this(windowSize, new ArrayList<Polygon>());
    }

    public World(Dimension windowSize, List<Polygon> holes) {
        this.windowSize = windowSize;
        this.holes = holes;
    }

    public final Dimension windowSize;
    private final List<Polygon> holes;

    public boolean hitsHole(Point pt)
    {
        for (Polygon o : holes)
            if (o.contains(pt))
                return true;
        return false;
    }

    public boolean hitsObstacle(Point pt)
    {
        return hitsHole(pt) ||
               pt.x < 31 || pt.x >= (windowSize.width - 31) ||
               pt.y < 73 || pt.y >= (windowSize.height - 23);
    }
}

