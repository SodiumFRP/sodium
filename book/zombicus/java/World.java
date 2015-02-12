import java.awt.Dimension;
import java.awt.Point;

public class World {
    public World(Dimension windowSize) {
        this.windowSize = windowSize;
    }

    final Dimension windowSize;

    public boolean hitsObstacle(Point pt)
    {
        return pt.x < 31 || pt.x >= (windowSize.width - 31) ||
               pt.y < 73 || pt.y >= (windowSize.height - 23);
    }
}

