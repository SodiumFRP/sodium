namespace Sodium
{
    /// <summary>
    ///     An interface representing an stream event listener.  This may be used to stop listening on a stream by calling
    ///     <see cref="Unlisten" />.
    /// </summary>
    public interface IListener
    {
        void Unlisten();

        IListenerWithWeakReference GetListenerWithWeakReference();
    }
}