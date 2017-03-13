using System.Windows;
using System.Windows.Media;
using Sodium;

namespace Fridgets
{
    public class FrTranslate : Fridget
    {
        public FrTranslate(Fridget fr, DiscreteCell<Point> offset)
            : base((size, sMouse, sKey, focus, idSupply) =>
            {
                Stream<MouseEvent> sMouseNew =
                    sMouse.Snapshot(offset, (e, o) =>
                        new MouseEvent(e.Args, () =>
                        {
                            Point p = e.GetPosition();
                            return new Point(p.X - o.X, p.Y - o.Y);
                        }));
                Output fo = fr.Reify(size, sMouseNew, sKey, focus, idSupply);
                DiscreteCell<DrawableDelegate> drawableNew = fo.Drawable.Lift(offset,
                    (dr, o) => new DrawableDelegate(d =>
                    {
                        d.PushTransform(new TranslateTransform(o.X, o.Y));
                        dr(d);
                        d.Pop();
                    }));
                return new Output(drawableNew, fo.DesiredSize, fo.SChangeFocus);
            })
        {
        }
    }
}