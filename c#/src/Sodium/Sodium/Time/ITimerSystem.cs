using System;

namespace Sodium.Time
{
    public interface ITimerSystem<T>
        where T : IComparable<T>
    {
        /// <summary>
        ///     Gets a cell giving the current clock time.
        /// </summary>
        Cell<T> Time { get; }

        /// <summary>
        ///     A timer that fires at the specified time.
        /// </summary>
        /// <param name="t">The time to fire at.</param>
        /// <returns>A stream which fires at the specified time.</returns>
        Stream<T> At(DiscreteCell<Maybe<T>> t);
    }
}