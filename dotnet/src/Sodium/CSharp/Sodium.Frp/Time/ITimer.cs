using System;

namespace Sodium.Frp.Time
{
    /// <summary>
    ///     An interface for a handle to cancel a timer.
    /// </summary>
    /// <remarks>
    ///     Disposing of the timer has the same effect as calling <see cref="Cancel" />.
    ///     Only one or the other needs to be called to cancel the timer.
    ///     Otherwise, objects implementing this interface do not need to be disposed.
    /// </remarks>
    public interface ITimer : IDisposable
    {
        void Cancel();
    }
}