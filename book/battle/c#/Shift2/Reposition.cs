using System;
using System.Windows;
using System.Windows.Shapes;

namespace Shift2
{
    public class Reposition
    {
        private readonly Lazy<Tuple<Polygon, Point>> output;

        public Reposition(DragInfo dragInfo, MouseEvt me, bool axisLock)
        {
            double tx = me.Pt.X - dragInfo.Me.Pt.X;
            double ty = me.Pt.Y - dragInfo.Me.Pt.Y;
            if (axisLock)
            {
                if (Math.Abs(tx) < Math.Abs(ty))
                {
                    tx = 0;
                }
                else
                {
                    ty = 0;
                }
            }
            this.output = new Lazy<Tuple<Polygon, Point>>(() => Tuple.Create(dragInfo.Me.Element.Polygon, new Point(dragInfo.OriginalLeft + tx, dragInfo.OriginalTop + ty)));
        }

        public Polygon Polygon => this.output.Value.Item1;
        public double Left => this.output.Value.Item2.X;
        public double Top => this.output.Value.Item2.Y;
    }
}