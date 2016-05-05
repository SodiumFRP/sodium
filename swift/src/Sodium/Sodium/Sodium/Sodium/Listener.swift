/// <summary>
///     A listener which runs the specified action when it is disposed.
/// </summary>
public class Listener : ListenerType
{
    private let _unlisten: Block

    /// <summary>
    ///     Creates a listener which runs the specified action when it is disposed.
    /// </summary>
    /// <param name="unlisten">The action to run when this listener should stop listening.</param>
    init(unlisten: Block)
    {
        self._unlisten = unlisten
    }

    public var hashValue: Int { return 0 }

    public func unlisten() {
        self._unlisten()
    }
}

public func ==(lhs: Listener, rhs: Listener) -> Bool {
    return lhs === rhs
}