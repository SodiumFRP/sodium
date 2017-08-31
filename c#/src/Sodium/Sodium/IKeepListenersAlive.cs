namespace Sodium
{
    internal interface IKeepListenersAlive
    {
        void KeepListenerAlive(IListener listener);
        void StopKeepingListenerAlive(IListener listener);
        void Use(IKeepListenersAlive childKeepListenersAlive);
    }
}