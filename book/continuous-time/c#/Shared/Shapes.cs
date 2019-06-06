using System.Windows;
using System.Windows.Media;
using Sodium.Frp;

namespace Shared
{
    public class Shapes
    {
        public static Behavior<DrawableDelegate> Circle(Color color)
        {
            return Behavior.Constant(new DrawableDelegate((d, h, o, s) =>
            {
                double radius = s;
                d.DrawEllipse(new SolidColorBrush(color), new Pen(Brushes.Black, 1), new Point(o.X, h - o.Y), radius, radius);
            }));
        }

        public static Behavior<DrawableDelegate> Scale(Behavior<DrawableDelegate> drawable, Behavior<double> scale)
        {
            return drawable.Lift(scale, (dr, ns) =>
                new DrawableDelegate((d, h, o, s) => dr(d, h, o, s * ns)));
        }

        public static Behavior<DrawableDelegate> Translate(Behavior<DrawableDelegate> drawable, Behavior<Point> offset)
        {
            return drawable.Lift(offset, (dr, no) =>
                new DrawableDelegate((d, h, o, s) => dr(d, h, no.Add(o.Multiply(s)), s)));
        }

        public static Behavior<DrawableDelegate> Over(Behavior<DrawableDelegate> a, Behavior<DrawableDelegate> b)
        {
            return a.Lift(b, (dra, drb) => new DrawableDelegate((d, h, o, s) =>
            {
                drb(d, h, o, s);
                dra(d, h, o, s);
            }));
        }
    }
}