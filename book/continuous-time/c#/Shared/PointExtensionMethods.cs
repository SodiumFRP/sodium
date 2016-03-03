using System.Windows;

namespace Shared
{
    public static class PointExtensionMethods
    {
        public static Point Add(this Point p, Point other)
        {
            return new Point(p.X + other.X, p.Y + other.Y);
        }

        public static Point Subtract(this Point p, Point other)
        {
            return new Point(p.X - other.X, p.Y - other.Y);
        }

        public static Point Multiply(this Point p, double a)
        {
            return new Point(p.X * a, p.Y * a);
        }
    }
}