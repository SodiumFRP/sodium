public class Point {
    Point(double x, double y) { this.x = x; this.y = y; }
    public final double x;
    public final double y;
    public Point add(Point other) {
        return new Point(x + other.x, y + other.y);
    }
    public Point subtract(Point other) {
        return new Point(x - other.x, y - other.y);
    }
    public Point multiply(double a) {
        return new Point(x * a, y * a);
    }
}

