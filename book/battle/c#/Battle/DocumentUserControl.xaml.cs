using System.Collections.Generic;
using System.Windows;

namespace Battle
{
    public partial class DocumentUserControl
    {
        public DocumentUserControl(UIElement container, IEnumerable<Element> elements, IParadigm paradigm)
        {
            this.InitializeComponent();

            foreach (Element element in elements)
            {
                this.Canvas.Children.Add(element.Polygon);
                element.Polygon.MouseDown += (sender, args) => paradigm.HandleMouseDown(new MouseEvtWithElement(element, args.GetPosition(this)));
                container.MouseMove += (sender, args) => paradigm.HandleMouseMove(new MouseEvt(args.GetPosition(this)));
                container.MouseUp += (sender, args) => paradigm.HandleMouseUp(new MouseEvt(args.GetPosition(this)));
            }
        }
    }
}