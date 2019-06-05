using System;

namespace Sodium.Frp.Time
{
    /// <summary>
    ///     An interface for implementations of FRP timer systems.
    /// </summary>
    /// <typeparam name="T">The underlying type of the timer's time values.</typeparam>
    public interface ITimerSystemImplementation<T>
    {
        void Start(Action<Exception> handleException);

        /// <summary>
        ///     Set a timer that will execute the specified callback at the specified time.
        /// </summary>
        /// <param name="t">The time at which to execute the callback.</param>
        /// <param name="callback">The callback to execute.</param>
        /// <returns>A handle that can be used to cancel the timer.</returns>
        ITimer SetTimer(T t, Action callback);

        /// <summary>
        ///     Run all pending timers scheduled for up to and including the specified time.
        /// </summary>
        void RunTimersTo(T t);

        /// <summary>
        ///     Return the current clock time.
        /// </summary>
        T Now { get; }
    }
}