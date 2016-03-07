using System.Windows;
using System.Windows.Media;
using Sodium;

namespace Shared
{
    public class Animate : UIElement
    {
        private readonly CompositionTargetSecondsTimerSystem timerSystem;
        private readonly Cell<DrawableDelegate> drawable;
        private readonly Size size;

        public Animate(AnimationDelegate animation, Size size)
        {
            this.timerSystem = CompositionTargetSecondsTimerSystem.Create(e => this.Dispatcher.Invoke(() => { throw e; }));
            Point extents = new Point(size.Width / 2, size.Height / 2);
            this.drawable = Transaction.Run(() =>
                Shapes.Translate(
                    animation(this.timerSystem, extents),
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
            animate.timerSystem.AddAnimation(animate);
        }
    }
}