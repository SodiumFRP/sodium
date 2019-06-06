using System;
using System.Collections.Concurrent;
using System.Windows.Media;
using Sodium.Frp.Time;

namespace Shared
{
    public class CompositionTargetTimerSystem : TimerSystem<TimeSpan>
    {
        private readonly ConcurrentBag<Animate> animationsToRun = new ConcurrentBag<Animate>();

        private CompositionTargetTimerSystem(Implementation implementation, Action<Exception> handleException)
            : base(implementation, handleException)
        {
            CompositionTarget.Rendering += (sender, args) =>
            {
                implementation.SetNow(((RenderingEventArgs)args).RenderingTime);
                foreach (Animate animation in this.animationsToRun.ToArray())
                {
                    animation.InvalidateVisual();
                }
            };
        }

        public void AddAnimation(Animate animation) => this.animationsToRun.Add(animation);

        public static CompositionTargetTimerSystem Create(TimeSpan startTime, Action<Exception> handleException) => new CompositionTargetTimerSystem(new Implementation(startTime), handleException);

        private class Implementation : TimerSystemImplementationImplementationBase<TimeSpan>
        {
            private TimeSpan now;
            private readonly object nowLock = new object();

            public Implementation(TimeSpan startTime)
            {
                this.now = startTime;
            }

            protected override TimeSpan SubtractTimes(TimeSpan first, TimeSpan second) => first - second;

            public override TimeSpan Now
            {
                get
                {
                    lock (this.nowLock)
                    {
                        return this.now;
                    }
                }
            }

            public void SetNow(TimeSpan now)
            {
                lock (this.nowLock)
                {
                    this.now = now;
                }
            }
        }
    }
}