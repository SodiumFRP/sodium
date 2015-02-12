import java.awt.Point;

public class Vector
{
    public Vector(double dx, double dy) {
        this.dx = dx;
        this.dy = dy;
    }
    public double dx;
    public double dy;
    public Vector mult(double c) {
        return new Vector(dx * c, dy * c);
    }
    public Point add(Point p) {
        return new Point(p.x + (int)dx, p.y + (int)dy);
    }
    public double magnitude() {
        return Math.sqrt(dx*dx + dy*dy);
    }
    public Vector normalize() {
        return mult(1/magnitude());
    }
    public static Vector subtract(Point a, Point b) {
        return new Vector(a.x - b.x, a.y - b.y);
    }
    public static double distance(Point a, Point b) {
        return Vector.subtract(a, b).magnitude();
    }
}
