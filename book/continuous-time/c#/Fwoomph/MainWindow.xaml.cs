using System;
using System.Windows.Media;
using Shared;
using Sodium.Frp;

namespace Fwoomph
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
                    Behavior<double> time = sys.Time;
                    double maxSize = 200.0;
                    return Shapes.Scale(
                        Shapes.Circle(Colors.Green),
                        time.Map(t =>
                        {
                            double frac = t - Math.Floor(t);
                            return (frac < 0.5 ? frac : 1.0 - frac) * maxSize;
                        }));
                }, this.Placeholder.RenderSize);
                this.Placeholder.Children.Add(animate);
                animate.Start();
            };
        }
    }
}