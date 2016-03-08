using System;
using System.Windows;
using System.Windows.Media;
using Shared;
using Sodium;

namespace Cross
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            this.Loaded += (sender, args) =>
            {
                Animate animate = new Animate((sys, extents) =>
                {
                    Cell<double> time = sys.Time;
                    double maxSize = 105;
                    Cell<double> offset = time.Map(t =>
                    {
                        double frac = t - Math.Floor(t);
                        return (frac < 0.5 ? frac - 0.25 : 0.75 - frac) * 4.0 * maxSize;
                    });
                    Cell<double> fifty = Cell.Constant(50.0);
                    Cell<DrawableDelegate> greenBall = Shapes.Translate(
                        Shapes.Scale(Shapes.Circle(Colors.Green), fifty),
                        offset.Map(x => new Point(x, 0.0)));
                    Cell<DrawableDelegate> blueBall = Shapes.Translate(
                        Shapes.Scale(Shapes.Circle(Colors.Blue), fifty),
                        offset.Map(y => new Point(0.0, y)));
                    return Shapes.Over(greenBall, blueBall);
                }, this.Placeholder.RenderSize);
                this.Placeholder.Children.Add(animate);
                animate.Start();
            };
        }
    }
}