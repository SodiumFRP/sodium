using System;
using System.Collections.Concurrent;
using System.Windows.Media;
using Sodium.Frp.Time;

namespace Shared
{
    public class CompositionTargetSecondsTimerSystem : TimerSystem<double>
    {
        private readonly ConcurrentBag<Animate> animationsToRun = new ConcurrentBag<Animate>();

        private CompositionTargetSecondsTimerSystem(Implementation implementation, Action<Exception> handleException)
            : base(implementation, handleException)
        {
            CompositionTarget.Rendering += (sender, args) =>
            {
                implementation.SetNow(((RenderingEventArgs)args).RenderingTime.TotalSeconds);
                foreach (Animate animation in this.animationsToRun.ToArray())
                {
                    animation.InvalidateVisual();
                }
            };
        }

        public void AddAnimation(Animate animation) => this.animationsToRun.Add(animation);

        public static CompositionTargetSecondsTimerSystem Create(double startTime, Action<Exception> handleException) => new CompositionTargetSecondsTimerSystem(new Implementation(startTime), handleException);

        private class Implementation : TimerSystemImplementationImplementationBase<double>
        {
            private double now;
            private readonly object nowLock = new object();

            public Implementation(double startTime)
            {
                this.now = startTime;
            }

            protected override TimeSpan SubtractTimes(double first, double second) => TimeSpan.FromSeconds(first - second);

            public override double Now
            {
                get
                {
                    lock (this.nowLock)
                    {
                        return this.now;
                    }
                }
            }

            public void SetNow(double now)
            {
                lock (this.nowLock)
                {
                    this.now = now;
                }
            }
        }
    }
}