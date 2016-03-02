using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;

namespace Shift
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            Func<int, int, int, double, Polygon> shape = (ox, oy, sides, angle) =>
            {
                Polygon polygon = new Polygon
                {
                    Fill = new SolidColorBrush(new Color { R = 64, G = 128, B = 0, A = 255 }),
                    Stroke = new SolidColorBrush(Colors.Black),
                    StrokeThickness = 1
                };
                angle *= Math.PI / 180.0;
                for (int i = 0; i < sides; i++)
                {
                    double theta = angle + Math.PI * 2 * i / sides;
                    polygon.Points.Add(new Point(ox + Math.Sin(theta) * 25, oy + Math.Cos(theta) * 25));
                }
                return polygon;
            };

            Func<IReadOnlyList<Element>> createElements = () => new[]
            {
                new Element("triangle",shape(50,50,3,180.0)),
                new Element("square",shape(125,50,4,225.0)),
                new Element("pentagon",shape(200,50,5,180.0)),
                new Element("hexagon",shape(50,125,6,210.0)),
                new Element("heptagon",shape(125,125,7,180.0)),
                new Element("octagon",shape(200,125,8,202.5))
            };

            this.ClassicPlaceholder.Child = new DocumentUserControl(this, this.ClassicPlaceholder, createElements(), new Classic(this.AddMessage));
            this.FrpPlaceholder.Child = new DocumentUserControl(this, this.FrpPlaceholder, createElements(), new Frp(this.AddMessage));
            this.ActorPlaceholder.Child = new DocumentUserControl(this, this.ActorPlaceholder, createElements(), new Actor(this.AddMessage, this.Dispatcher));
        }

        private void AddMessage(string message)
        {
            this.StackPanel.Children.Add(new TextBlock { Text = message });
            this.ScrollViewer.ScrollToBottom();
        }
    }
}