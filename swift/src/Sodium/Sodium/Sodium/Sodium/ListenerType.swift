/*
<summary>
     An interface representing an stream event listener.  This may be used to stop listening on a stream by either
     disposing
     it or calling <see cref="Unlisten" />.
 </summary>
 <remarks>
     Disposing of the listener has the same effect as calling <see cref="Unlisten" />.
     Only one or the other needs to be called to cause the listener to stop listening.
     Otherwise, objects implementing this interface do not need to be disposed.
 </remarks>
*/
public protocol ListenerType: Hashable
{
    func unlisten()
}
