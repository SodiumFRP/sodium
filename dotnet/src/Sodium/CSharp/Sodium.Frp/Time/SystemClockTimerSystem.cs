using System;

namespace Sodium.Frp.Time
{
    /// <summary>
    ///     A timer system using the current system clock.
    /// </summary>
    public class SystemClockTimerSystem : TimerSystem<DateTime>
    {
        public SystemClockTimerSystem(Action<Exception> handleException)
            : base(new Implementation(), handleException)
        {
        }

        internal class Implementation : TimerSystemImplementationImplementationBase<DateTime>
        {
            protected override TimeSpan SubtractTimes(DateTime first, DateTime second) => first - second;
            public override DateTime Now => DateTime.Now;
        }
    }
}