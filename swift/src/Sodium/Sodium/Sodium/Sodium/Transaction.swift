typealias Block = () -> Void
typealias Action = () throws -> Void
typealias TV = (Transaction) throws -> Void
typealias OTV = (Transaction?) throws -> Void

let nop: Block = {}

/// <summary>
///     A class for managing transactions.
/// </summary>
public final class Transaction
{
    // Coarse-grained lock that's held during the whole transaction.
    private static let transactionLock = NSObject()
    
    
    private static var currentTransaction: Transaction?
    private static var OnStartHooks = Array<Action>()
    private static var runningOnStartHooks: Bool = false

    private var entries = Set<Entry>()
    private var lastQueue = Array<Action>()
    private var postQueue = Dictionary<Int, OTV>()

    private let prioritizedQueue = PriorityQueue<Entry>(comparator: <)
    static var inCallback = 0
    
    // True if we need to re-generate the priority queue.
    private var toRegen = false

    init() {
    }
    
    /// <summary>
    ///     Return the current transaction as an option type.
    /// </summary>
    /// <returns>The current transaction as an option type.</returns>
    internal static func getCurrentTransaction() -> Transaction?
    {
        objc_sync_enter(transactionLock)
        defer { objc_sync_exit(transactionLock) }

        return currentTransaction
    }

    /// <summary>
    ///     Return whether or not there is a current transaction.
    /// </summary>
    /// <returns><code>true</code> if there is a current transaction, <code>false</code> otherwise.</returns>
    internal static func hasCurrentTransaction() -> Bool
    {
        objc_sync_enter(transactionLock)
        defer { objc_sync_exit(transactionLock) }

        return currentTransaction != nil
    }

    /// <summary>
    ///     Execute the specified action inside a single transaction.
    /// </summary>
    /// <param name="action">The action to execute.</param>
    /// <remarks>
    ///     In most cases this is not needed, because all primitives will create their own transaction automatically.
    ///     It is useful for running multiple reactive operations atomically.
    /// </remarks>
    internal static func runVoid(action: Action) {
        go { try action() }
    }

    public static func noThrowRun<T>(f: () -> T) -> T {
        return go { f() }!
    }
    
    /// <summary>
    ///     Execute the specified function inside a single transaction.
    /// </summary>
    /// <typeparam name="T">The type of the value returned.</typeparam>
    /// <param name="f">The function to execute.</param>
    /// <returns>The return value of <paramref name="f" />.</returns>
    /// <remarks>
    ///     In most cases this is not needed, because all primitives will create their own transaction automatically.
    ///     It is useful for running multiple reactive operations atomically.
    /// </remarks>
    public static func run<T>(f: () throws -> T) -> T?
    {
        return go { try f() }
    }

    internal static func run(code: TV) {
        go( { try code(startIfNecessary())})
    }

    static func go<R>(code: () throws -> R) -> R? {
        objc_sync_enter(transactionLock)
        defer { objc_sync_exit(transactionLock) }
        
        // If we are already inside a transaction (which must be on the same
        // thread otherwise we wouldn't have acquired transactionLock), then
        // keep using that same transaction.
        let transWas = currentTransaction
        defer
        {
            do
            {
                if (transWas == nil) {
                    try currentTransaction?.close()
                }
            }
            catch
            {
            }
            currentTransaction = transWas
        }

        do
        {
            return try code()
        }
        catch
        {
        }
        return nil
    }

    internal static func apply<T>(code: (Transaction) -> T) -> T {
        return go { code(startIfNecessary()) }!
    }

    internal static func apply<T>(code: (Transaction) throws -> T) -> T? {
        return go { try code(startIfNecessary()) }
    }

    /// <summary>
    ///     Add an action that will be executed whenever a transaction is started.
    /// </summary>
    /// <param name="action"></param>
    /// <remarks>
    ///     The action may start transactions itself, which will not cause the hooks to execute recursively.
    ///     The main use case of this is for the implementation of a time/alarm system.
    /// </remarks>
    internal static func onStart(action: Action) {
        objc_sync_enter(transactionLock)
        defer { objc_sync_exit(transactionLock) }

        OnStartHooks.append(action)
    }

    private static func startIfNecessary() -> Transaction {
        if (currentTransaction == nil)
        {
            if (!runningOnStartHooks)
            {
                runningOnStartHooks = true
                do
                {
                    for action in OnStartHooks
                    {
                        try action()
                    }
                }
                catch
                {
                    runningOnStartHooks = false
                }
            }

            currentTransaction = Transaction()
        }
        return currentTransaction!
    }

    internal func prioritized(rank: INode, action: TV) {
        let e = Entry(rank: rank, action: action)
        self.prioritizedQueue.push(e)
        self.entries.insert(e)
    }

    /// <summary>
    ///     Add an action to run after all prioritized actions.
    /// </summary>
    /// <param name="action">The action to run after all prioritized actions.</param>
    internal func last(action: Action) {
        self.lastQueue.append(action)
    }

    /// <summary>
    ///     Add an action to run after all last actions.
    /// </summary>
    /// <param name="index">The order index in which to run the action.</param>
    /// <param name="action">The action to run after all last actions.</param>
    internal func post(index: Int, action: OTV) {
        // If an entry exists already, combine the old one with the new one.
        var new: OTV
        if let existing = self.postQueue[index] {
            new = { trans in
                try existing(trans)
                try action(trans)
            }
        }
        else {
            new = action
        }

        self.postQueue[index] = new
    }

    /// <summary>
    ///     Execute an action after the current transaction is closed
    ///     or immediately if there is no current transaction.
    /// </summary>
    /// <param name="action">
    ///     The action to run after the current transaction is closed
    ///     or immediately if there is no current transaction.
    /// </param>
    internal static func post(action: OTV) {
        // -1 will mean it runs before anything split/deferred, and will run
        // outside a transaction context.
        self.run { trans in trans.post(-1, action: action) }
    }

    internal func setNeedsRegenerating() {
        self.toRegen = true
    }

    // If the priority queue has entries in it when we modify any of the nodes'
    // ranks, then we need to re-generate it to make sure it's up-to-date.
    private func checkRegen() {
        if (self.toRegen)
        {
            self.toRegen = false
            self.prioritizedQueue.removeAll()
            for e in self.entries {
                self.prioritizedQueue.push(e)
            }
        }
    }

    internal func close() throws {
        while (true)
        {
            self.checkRegen()

            if self.prioritizedQueue.isEmpty {
                break
            }

            let e = self.prioritizedQueue.pop()
            self.entries.remove(e!)
            try e!.Action(self)
        }

        for action in self.lastQueue {
            try action()
        }
        self.lastQueue.removeAll()

        for pair in self.postQueue {
            let parent = Transaction.currentTransaction
            do
            {
                if (pair.0 < 0)
                {
                    Transaction.currentTransaction = nil
                    try pair.1(nil)
                }
                else
                {
                    let transaction = Transaction()
                    Transaction.currentTransaction = transaction
                    do
                    {
                        try pair.1(transaction)
                    }
                    catch
                    {
                        try transaction.close()
                    }
                }
            }
            catch
            {
                Transaction.currentTransaction = parent
            }
        }
        self.postQueue.removeAll()
    }

    class Entry : Sequenced, Comparable, Hashable
    {
        //private static var nextSeq: Int64 = 0

        let Rank: INode
        let Action: TV
        let seq: Int64
        
        var hashValue: Int { return Int(seq) }

        init(rank: INode, action: TV) {
            //super.init()
            
            self.Rank = rank
            self.Action = action
            self.seq = Int64(0)//next()
        }
    }
}

func ==(lhs: Transaction.Entry, rhs: Transaction.Entry) -> Bool {
    return lhs.Rank == rhs.Rank && lhs.seq == rhs.seq
}
func <(lhs: Transaction.Entry, rhs: Transaction.Entry) -> Bool {
    if lhs.Rank < rhs.Rank { return true }
    if lhs.Rank == rhs.Rank && lhs.seq < rhs.seq { return true }
    return false
}
func <=(lhs: Transaction.Entry, rhs: Transaction.Entry) -> Bool {
    if lhs.Rank < rhs.Rank { return true }
    if lhs.Rank == rhs.Rank && lhs.seq <= rhs.seq { return true }
    return false
}
func >=(lhs: Transaction.Entry, rhs: Transaction.Entry) -> Bool {
    if lhs.Rank > rhs.Rank { return true }
    if lhs.Rank == rhs.Rank && lhs.seq >= rhs.seq { return true }
    return false
}
func >(lhs: Transaction.Entry, rhs: Transaction.Entry) -> Bool {
    if lhs.Rank > rhs.Rank { return true }
    if lhs.Rank == rhs.Rank && lhs.seq > rhs.seq { return true }
    return false
}

protocol Sequenced {
    func next() -> Int64
}

var nextSeq = Int64(0)
extension Sequenced {
    
    func next() -> Int64 {
        return OSAtomicIncrement64(&nextSeq)
    }
}

enum SodiumError: ErrorType {
    case InvalidOperation
}