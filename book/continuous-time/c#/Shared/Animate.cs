using System;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Media;
using Sodium;
using Sodium.Time;

namespace Shared
{
    public class Animate : UIElement
    {
        private readonly Cell<DrawableDelegate> drawable;
        private readonly Size size;

        public Animate(AnimationDelegate animation, Size size)
        {
            Point extents = new Point(size.Width / 2, size.Height / 2);
            this.drawable = Transaction.Run(() =>
                Shapes.Translate(
                    animation(new SecondsTimerSystem(e => this.Dispatcher.Invoke(() => { throw e; })), extents),
                    Cell.Constant(extents)));
            this.size = size;
        }

        protected override void OnRender(DrawingContext drawingContext)
        {
            base.OnRender(drawingContext);

            this.drawable.Sample()(drawingContext, this.size.Height, new Point(0, 0), 1.0);
        }

        public static void RunAnimation(Animate animate)
        {
            Task.Run(() =>
            {
                DateTime tLast = DateTime.Now;
                while (true)
                {
                    DateTime t = DateTime.Now;
                    DateTime tIdeal = tLast + TimeSpan.FromMilliseconds(15);
                    TimeSpan toWait = tIdeal - t;
                    if (toWait > TimeSpan.Zero)
                    {
                        Thread.Sleep(toWait);
                    }
                    Transaction.RunVoid(() => { });
                    animate.Dispatcher.Invoke(animate.InvalidateVisual);
                    tLast = tIdeal;
                }
                // ReSharper disable once FunctionNeverReturns
            });
        }
    }
}