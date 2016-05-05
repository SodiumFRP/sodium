internal protocol IKeepListenersAlive
{
    func KeepListenerAlive(listener: Listener)
    func StopKeepingListenerAlive(listener : Listener)
    func Use(childKeepListenersAlive: IKeepListenersAlive)
}
