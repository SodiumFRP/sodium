internal class INode : NSObject, Comparable
{
    static let Null = INode(rank: 0x123456789)
    
    // Fine-grained lock that protects listeners and nodes.
    internal static let ListenersLock = NSObject()

    private var _rank: Int64

    internal init(rank: Int64) {
        self._rank = rank
    }

    //override var hashValue: Int { return super.hashValue }
    
    internal var rank: Int64 { return self._rank }

    internal static func ensureBiggerThan(node: INode, limit: Int64, inout visited: Set<INode>) -> Bool {
        if (node.rank > limit || visited.contains(node))
        {
            return false
        }

        visited.insert(node)
        node._rank = limit + 1
        for n in node.getListenerNodesUnsafe()
        {
            ensureBiggerThan(n, limit: node.rank, visited: &visited)
        }

        return true
    }

    func getListenerNodesUnsafe() -> [INode] { return [] }

    class Target : NSObject
    {
        let node: INode

        init(node: INode)
        {
            self.node = node
        }
    }
}

func ==(lhs: INode, rhs: INode) -> Bool {
    return lhs.rank == rhs.rank
}
func <(lhs: INode, rhs: INode) -> Bool {
    return lhs.rank < rhs.rank
}
func <=(lhs: INode, rhs: INode) -> Bool {
    return lhs.rank <= rhs.rank
}
func >=(lhs: INode, rhs: INode) -> Bool {
    return lhs.rank >= rhs.rank
}
func >(lhs: INode, rhs: INode) -> Bool {
    return lhs.rank > rhs.rank
}

internal class Node<T> : INode
{
    typealias ACTION = (Transaction, T, String) -> Void
    
    private var listeners = Array<NodeTarget<T>>()

    internal override init(rank: Int64)
    {
        super.init(rank: rank)
    }

    /// <summary>
    ///     Link an action and a target node to this node.
    /// </summary>
    /// <param name="action">The action to link to this node.</param>
    /// <param name="target">The target node to link to this node.</param>
    /// <returns>
    ///     A tuple containing whether or not changes were made to the node rank
    ///     and the <see cref="Target" /> object created for this link.
    /// </returns>
    internal func link(action: ACTION, target: INode) -> (Bool, NodeTarget<T>) {
        objc_sync_enter(INode.ListenersLock)
        defer { objc_sync_exit(INode.ListenersLock) }

        var v = Set<INode>()
        let changed = INode.ensureBiggerThan(target, limit: self.rank, visited: &v)
        let t = NodeTarget(action: action, node: target)
        self.listeners.append(t)
        return (changed, t)
    }

    internal func unlink(target: NodeTarget<T>)
    {
        self.removeListener(target)
    }


    internal func getListeners() -> [NodeTarget<T>]
    {
        objc_sync_enter(INode.ListenersLock)
        defer { objc_sync_exit(INode.ListenersLock) }
        
        return self.listeners
    }

    internal func removeListener(target: NodeTarget<T>) {
        objc_sync_enter(INode.ListenersLock)
        defer { objc_sync_exit(INode.ListenersLock) }

        self.listeners.remove(target)
    }

    override func getListenerNodesUnsafe() -> [INode] {
        objc_sync_enter(INode.ListenersLock)
        defer { objc_sync_exit(INode.ListenersLock) }
        
        return self.listeners.map{ $0.node }
    }
    
    //override var hashValue: Int { return 3 }
}

class NodeTarget<T> : INode.Target
{
    typealias Action = (Transaction, T, String) -> Void
    
    internal var action: Action
    
    internal init(action: Action, node: INode)
    {
        self.action = action
        super.init(node: node)
    }
    
    //var hashValue: Int { return 9 }
}

func ==<T>(lhs: NodeTarget<T>, rhs: NodeTarget<T>) -> Bool {
    return lhs.hashValue == rhs.hashValue
}
