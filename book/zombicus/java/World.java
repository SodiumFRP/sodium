import java.awt.Point;

public class World {
    public World(Point windowSize) {
        this.windowSize = windowSize;
    }

    final Point windowSize;

    public boolean hitsObstacle(Point pt)
    {
        return pt.x < 31 || pt.x >= (windowSize.x - 31) ||
               pt.y < 73 || pt.y >= (windowSize.y - 23);
    }
}

