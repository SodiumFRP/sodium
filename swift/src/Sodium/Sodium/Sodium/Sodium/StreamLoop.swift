/// <summary>
///     A forward reference for a <see cref="Stream{T}" /> equivalent to the <see cref="Stream{T}" /> that is referenced.
/// </summary>
/// <typeparam name="T">The type of values fired by the stream.</typeparam>
public class StreamLoop<T> : Stream<T>
{
    private let isAssignedLock = NSObject()
    private var isAssigned = false

    /// <summary>
    ///     Create an <see cref="StreamLoop{T}" />.  This must be called from within a transaction.
    /// </summary>
    public override init()
    {
        if (!Transaction.HasCurrentTransaction())
        {
            fatalError("StreamLoop and CellLoop must be used within an explicit transaction")
        }
        super.init()
    }

    internal var IsAssigned: Bool
    {
        get
        {
            objc_sync_enter(self.isAssignedLock)
            defer { objc_sync_exit(self.isAssignedLock) }

            return self.isAssigned
        }
    }

    /// <summary>
    ///     Resolve the loop to specify what the <see cref="StreamLoop{T}" /> was a forward reference to.  This method
    ///     must be called inside the same transaction as the one in which this <see cref="StreamLoop{T}" /> instance was
    ///     created and used.
    ///     This requires an explicit transaction to be created with <see cref="Transaction.Run{T}(Func{T})" /> or
    ///     <see cref="Transaction.RunVoid(Action)" />.
    /// </summary>
    /// <param name="stream">The stream that was forward referenced.</param>
    public func Loop(stream: Stream<T>) {
        objc_sync_enter(self.isAssignedLock)
        defer { objc_sync_exit(self.isAssignedLock) }

        if (self.isAssigned) {
            fatalError("StreamLoop was looped more than once.")
        }

        self.isAssigned = true

        Transaction.RunVoid {
            self.UnsafeAddCleanup(stream.Listen(self.node, action: self.Send))
            stream.KeepListenersAlive.Use(self.KeepListenersAlive)
        }
    }
}
