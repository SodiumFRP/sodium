using System.Windows;

namespace Shift
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