using System;
using Sodium.Time;

namespace Shared
{
    public class CompositionTargetTimerSystem : TimerSystem<TimeSpan>
    {
        private readonly Implementation implementation;

        private CompositionTargetTimerSystem(Implementation implementation, Action<Exception> handleException)
            : base(implementation, handleException)
        {
            this.implementation = implementation;
        }

        public static CompositionTargetTimerSystem Create(Action<Exception> handleException) => new CompositionTargetTimerSystem(new Implementation(), handleException);
        public void SetNow(TimeSpan now) => this.implementation.SetNow(now);

        private class Implementation : TimerSystemImplementationImplementationBase<TimeSpan>
        {
            private TimeSpan now;
            private readonly object nowLock = new object();

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