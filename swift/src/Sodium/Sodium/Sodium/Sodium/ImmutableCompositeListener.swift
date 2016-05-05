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
        self.Unlisten()
    }

    public override func Unlisten() {
        for l in self.listeners {
            l.Unlisten()
        }
    }
}
