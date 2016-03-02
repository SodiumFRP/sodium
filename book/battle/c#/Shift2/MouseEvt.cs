using System.Windows;

namespace Shift2
{
    public class MouseEvt
    {
        public MouseEvt(Point pt)
        {
            this.Pt = pt;
        }

        public Point Pt { get; }
    }
}