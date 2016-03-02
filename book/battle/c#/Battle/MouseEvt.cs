using System.Windows;

namespace Battle
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