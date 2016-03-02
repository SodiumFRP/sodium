using System.Windows;

namespace Shift
{
    public class MouseEvtWithElement : MouseEvt
    {
        public MouseEvtWithElement(Element element, Point pt)
            : base(pt)
        {
            this.Element = element;
        }

        public Element Element { get; set; }
    }
}