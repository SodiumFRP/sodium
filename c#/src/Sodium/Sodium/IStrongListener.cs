using System;

namespace Sodium
{
    /// <summary>
    ///     An interface representing an stream event listener.  This may be used to stop listening on a stream by calling
    ///     <see cref="IListener.Unlisten" />.
    /// </summary>
    public interface IStrongListener : IListener, IDisposable
    {
    }
}