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

    public func Add(l: Listener) {
        self.listeners.append(l)
    }

    public func AddRange<S : SequenceType where S.Generator.Element == Listener>(ls: S) {
        self.listeners.appendContentsOf(ls)
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
