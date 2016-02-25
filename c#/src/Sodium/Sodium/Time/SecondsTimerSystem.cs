using System;

namespace Sodium.Time
{
    /// <summary>
    ///     A timer system using the number of seconds since the application started.
    /// </summary>
    public class SecondsTimerSystem : TimerSystem<double>
    {
        public SecondsTimerSystem(Action<Exception> handleException)
            : base(new Implementation(handleException))
        {
        }

        private class Implementation : ITimerSystemImplementation<double>
        {
            private readonly DateTime startTime;
            private readonly ITimerSystemImplementation<DateTime> implementation;

            public Implementation(Action<Exception> handleException)
            {
                this.implementation = new SystemClockTimerSystem.Implementation(handleException);
                this.startTime = this.implementation.Now;
            }

            public ITimer SetTimer(double time, Action callback) => this.implementation.SetTimer(this.startTime + TimeSpan.FromSeconds(time * 1000.0), callback);
            public void RunTimersTo(double now) => this.implementation.RunTimersTo(this.startTime + TimeSpan.FromSeconds(now * 1000.0));
            public double Now => (this.implementation.Now - this.startTime).TotalSeconds;
        }
    }
}