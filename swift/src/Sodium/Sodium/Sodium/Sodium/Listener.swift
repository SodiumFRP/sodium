/* <summary>
 *     A listener which runs the specified action when it is disposed.
 * </summary>
 */
public class Listener : NSObject, ListenerType
{
    private let _unlisten: Block

    // <summary>
    //     Creates a listener which runs the specified action when it is disposed.
    // </summary>
    // <param name="unlisten">The action to run when this listener should stop listening.</param>
    init(unlisten: Block)
    {
        self._unlisten = unlisten
    }

    public override var hashValue: Int { return super.hashValue }

    public func unlisten() {
        self._unlisten()
    }
}

public func ==(lhs: Listener, rhs: Listener) -> Bool {
    return lhs.hashValue == rhs.hashValue
}