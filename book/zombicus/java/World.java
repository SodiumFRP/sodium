import java.awt.Dimension;
import java.awt.Point;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.List;

public class World {
    public World(Dimension windowSize) {
        this(windowSize, new ArrayList<Polygon>());
    }

    public World(Dimension windowSize, List<Polygon> obstacles) {
        this.windowSize = windowSize;
        this.obstacles = obstacles;
    }

    private final Dimension windowSize;
    private final List<Polygon> obstacles;

    public boolean hitsObstacle(Point pt)
    {
        for (Polygon o : obstacles) {
            if (o.contains(pt))
                return true;
        }
        return pt.x < 31 || pt.x >= (windowSize.width - 31) ||
               pt.y < 73 || pt.y >= (windowSize.height - 23);
    }
}

