namespace Sodium
{
    /// <summary>
    ///     An interface representing an stream event listener which does not keep the stream from being garbage collected.
    ///     This may be used to stop listening on a stream by calling <see cref="Unlisten" />.
    /// </summary>
    public interface IListenerWithWeakReference
    {
        void Unlisten();
    }
}