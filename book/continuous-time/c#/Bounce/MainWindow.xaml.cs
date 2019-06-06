using System.Windows;
using System.Windows.Media;
using Shared;
using Sodium.Frp;
using Sodium.Frp.Time;

namespace Bounce
{
    public partial class MainWindow
    {
        private static readonly double Restitution = 0.95;

        public MainWindow()
        {
            this.InitializeComponent();

            this.Loaded += (sender, args) =>
            {
                Animate animate = new Animate((sys, extents) =>
                {
                    Behavior<double> time = sys.Time;
                    double t0 = time.Sample();
                    double ballRadius = 15;
                    double leftWall = -extents.X + ballRadius;
                    double rightWall = extents.X - ballRadius;
                    double floor = -extents.Y + ballRadius;
                    double roof = extents.Y - ballRadius;
                    Signal gravity = new Signal(t0, 0, 0, -1200);
                    StreamLoop<Signal> sBounceX = new StreamLoop<Signal>();
                    StreamLoop<Signal> sBounceY = new StreamLoop<Signal>();
                    Cell<Signal> velx = sBounceX.Hold(new Signal(t0, 0, 0, 350));
                    Cell<Signal> vely = sBounceY.Hold(gravity.Integrate(0));
                    Cell<Signal> posx = Signal.Integrate(velx, leftWall);
                    Cell<Signal> posy = Signal.Integrate(vely, roof);
                    sBounceX.Loop(BounceAt(sys, velx, posx, leftWall).OrElse(BounceAt(sys, velx, posx, rightWall)));
                    sBounceY.Loop(BounceAt(sys, vely, posy, floor));
                    return Shapes.Translate(Shapes.Scale(Shapes.Circle(Colors.Red), Behavior.Constant(ballRadius)), time.Lift(posx.AsBehavior(), posy.AsBehavior(), (t, x, y) => new Point(x.ValueAt(t), y.ValueAt(t))));
                }, this.Placeholder.RenderSize);
                this.Placeholder.Children.Add(animate);
                animate.Start();
            };
        }

        private static Stream<Signal> BounceAt(TimerSystem<double> sys,
            Cell<Signal> vel, Cell<Signal> pos, double target)
        {
            return sys.At(pos.Map(p => p.When(target))).Snapshot(vel, (t, v) =>
                new Signal(t, v.A, v.B, -v.ValueAt(t) * Restitution));
        }
    }
}