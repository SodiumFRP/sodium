using System;
using Sodium.Functional;

namespace Sodium.Frp.Time
{
    public interface ITimerSystem<T>
        where T : IComparable<T>
    {
        /// <summary>
        ///     Gets a behavior giving the current clock time.
        /// </summary>
        Behavior<T> Time { get; }

        /// <summary>
        ///     A timer that fires at the specified time.
        /// </summary>
        /// <param name="t">The time to fire at.</param>
        /// <returns>A stream which fires at the specified time.</returns>
        Stream<T> At(Cell<Maybe<T>> t);
    }
}