using System;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Media;
using Sodium.Frp;

namespace Shared
{
    public class Animate : UIElement
    {
        private readonly Task<Renderer> renderer;
        private readonly CellSink<bool> isStarted = Cell.CreateSink(false);

        public Animate(AnimationDelegate animation, Size size)
        {
            TaskCompletionSource<Renderer> tcs = new TaskCompletionSource<Renderer>();
            this.renderer = tcs.Task;

            void CreateTimerSystem(object sender, EventArgs args)
            {
                CompositionTargetSecondsTimerSystem timerSystem = CompositionTargetSecondsTimerSystem.Create(((RenderingEventArgs)args).RenderingTime.TotalSeconds, e => this.Dispatcher.Invoke(() => throw e));
                Point extents = new Point(size.Width / 2, size.Height / 2);
                tcs.SetResult(new Renderer(timerSystem, Transaction.Run(() => Shapes.Translate(animation(timerSystem, extents), Behavior.Constant(extents))), size, this, this.isStarted));
                CompositionTarget.Rendering -= CreateTimerSystem;
            }

            CompositionTarget.Rendering += CreateTimerSystem;
        }

        protected override void OnRender(DrawingContext drawingContext)
        {
            base.OnRender(drawingContext);

            if (this.renderer.IsCompleted)
            {
                this.renderer.Result.Render(drawingContext);
            }
        }

        public void Start()
        {
            this.isStarted.Send(true);
        }

        private class Renderer
        {
            private readonly Behavior<DrawableDelegate> drawable;
            private readonly Size size;

            public Renderer(CompositionTargetSecondsTimerSystem timerSystem, Behavior<DrawableDelegate> drawable, Size size, Animate animation, Cell<bool> isStarted)
            {
                this.drawable = drawable;
                this.size = size;

                Transaction.RunVoid(() => isStarted.Values().Filter(s => s).ListenOnce(_ => timerSystem.AddAnimation(animation)));
            }

            public void Render(DrawingContext drawingContext)
            {
                this.drawable.Sample()(drawingContext, this.size.Height, new Point(0, 0), 1.0);
            }
        }
    }
}