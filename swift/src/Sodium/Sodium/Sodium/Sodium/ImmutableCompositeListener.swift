public class ImmutableCompositeListener : Listener
{
    private let listeners: [Listener]

    public init(listeners: [Listener]?)
    {
        self.listeners = listeners ?? []
        super.init(unlisten: {})
    }

    deinit
    {
        self.unlisten()
    }

    public override func unlisten() {
        for l in self.listeners {
            l.unlisten()
        }
    }
}
