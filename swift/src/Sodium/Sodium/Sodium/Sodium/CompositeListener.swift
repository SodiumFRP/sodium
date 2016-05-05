public class CompositeListener : Listener
{
    private var listeners: [Listener]

    public convenience init()
    {
        self.init(listeners: nil)
    }

    public init(listeners: [Listener]?)
    {
        self.listeners = listeners ?? []
        super.init(unlisten: {})
    }

    public func add(l: Listener) {
        self.listeners.append(l)
    }

    public func addRange<S : SequenceType where S.Generator.Element == Listener>(ls: S) {
        self.listeners.appendContentsOf(ls)
    }

    public override func unlisten() {
        for l in self.listeners {
            l.unlisten()
        }
    }
}
