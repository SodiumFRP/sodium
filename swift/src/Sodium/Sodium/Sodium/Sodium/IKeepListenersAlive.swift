internal protocol IKeepListenersAlive
{
    func keepListenerAlive(listener: Listener)
    func stopKeepingListenerAlive(listener : Listener)
    func use(childKeepListenersAlive: IKeepListenersAlive)
}
