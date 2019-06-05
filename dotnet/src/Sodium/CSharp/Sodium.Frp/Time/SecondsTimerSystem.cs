using System;

namespace Sodium.Frp.Time
{
    /// <summary>
    ///     A timer system using the number of seconds since the application started.
    /// </summary>
    public sealed class SecondsTimerSystem : TimerSystem<double>
    {
        public SecondsTimerSystem(Action<Exception> handleException)
            : base(new Implementation(), handleException)
        {
        }

        private class Implementation : TimerSystemImplementationImplementationBase<double>
        {
            private readonly DateTime startTime;

            public Implementation() => this.startTime = DateTime.Now;

            protected override TimeSpan SubtractTimes(double first, double second) =>
                TimeSpan.FromSeconds(first - second);

            public override double Now => (DateTime.Now - this.startTime).TotalSeconds;
        }
    }
}