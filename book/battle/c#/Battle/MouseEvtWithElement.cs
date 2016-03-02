using System.Windows;

namespace Battle
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