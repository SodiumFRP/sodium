using System;
using System.Windows;
using System.Windows.Shapes;

namespace Battle
{
    public class Reposition
    {
        private readonly Lazy<Tuple<Polygon, Point>> output;

        public Reposition(DragInfo dragInfo, MouseEvt me)
        {
            this.output = new Lazy<Tuple<Polygon, Point>>(() => Tuple.Create(dragInfo.Me.Element.Polygon, new Point(dragInfo.OriginalLeft + me.Pt.X - dragInfo.Me.Pt.X, dragInfo.OriginalTop + me.Pt.Y - dragInfo.Me.Pt.Y)));
        }

        public Polygon Polygon => this.output.Value.Item1;
        public double Left => this.output.Value.Item2.X;
        public double Top => this.output.Value.Item2.Y;
    }
}