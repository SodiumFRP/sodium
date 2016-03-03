using System.Windows;
using System.Windows.Media;
using Sodium;

namespace Shared
{
    public class Shapes
    {
        public static Cell<DrawableDelegate> Circle(Color color)
        {
            return Cell.Constant(new DrawableDelegate((d, h, o, s) =>
            {
                double radius = s;
                d.DrawEllipse(new SolidColorBrush(color), new Pen(Brushes.Black, 1), new Point(o.X, h - o.Y), radius, radius);
            }));
        }

        public static Cell<DrawableDelegate> Scale(Cell<DrawableDelegate> drawable, Cell<double> scale)
        {
            return drawable.Lift(scale, (dr, ns) =>
                new DrawableDelegate((d, h, o, s) => dr(d, h, o, s * ns)));
        }

        public static Cell<DrawableDelegate> Translate(Cell<DrawableDelegate> drawable, Cell<Point> offset)
        {
            return drawable.Lift(offset, (dr, no) =>
                new DrawableDelegate((d, h, o, s) => dr(d, h, no.Add(o.Multiply(s)), s)));
        }

        public static Cell<DrawableDelegate> Over(Cell<DrawableDelegate> a, Cell<DrawableDelegate> b)
        {
            return a.Lift(b, (dra, drb) => new DrawableDelegate((d, h, o, s) =>
            {
                drb(d, h, o, s);
                dra(d, h, o, s);
            }));
        }
    }
}