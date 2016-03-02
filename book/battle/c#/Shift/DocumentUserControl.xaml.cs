using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Input;

namespace Shift
{
    public partial class DocumentUserControl
    {
        public DocumentUserControl(Window window, UIElement container, IEnumerable<Element> elements, IParadigm paradigm)
        {
            this.InitializeComponent();

            Func<KeyEventArgs, Key> getKey = e => e.Key == Key.System ? e.SystemKey : e.Key;

            foreach (Element element in elements)
            {
                this.Canvas.Children.Add(element.Polygon);

                element.Polygon.MouseDown += (sender, args) => paradigm.HandleMouseDown(new MouseEvtWithElement(element, args.GetPosition(this)));
                container.MouseMove += (sender, args) => paradigm.HandleMouseMove(new MouseEvt(args.GetPosition(this)));
                container.MouseUp += (sender, args) => paradigm.HandleMouseUp(new MouseEvt(args.GetPosition(this)));

                bool isLeftDown = false;
                bool isRightDown = false;

                window.KeyDown += (sender, args) =>
                {
                    Key key = getKey(args);

                    if (key == Key.LeftShift)
                    {
                        isLeftDown = true;
                    }

                    if (key == Key.RightShift)
                    {
                        isRightDown = true;
                    }

                    paradigm.HandleShift(isLeftDown || isRightDown);
                };

                window.KeyUp += (sender, args) =>
                {
                    Key key = getKey(args);

                    if (key == Key.LeftShift)
                    {
                        isLeftDown = false;
                    }

                    if (key == Key.RightShift)
                    {
                        isRightDown = false;
                    }

                    paradigm.HandleShift(isLeftDown || isRightDown);
                };
            }
        }
    }
}