/// <summary>
///     A stream that allows values to be pushed into it, acting as an interface between the world of I/O and the world of
///     FRP.  Code that exports StreamSinks for read-only use should downcast to <see cref="Stream{T}" />.
/// </summary>
/// <typeparam name="T">The type of values fired by the stream.</typeparam>
public class StreamSink<T> : Stream<T>
{
    //private let coalescer: (Transaction, T) -> Void
    private let fold: (T,T)->T
    /// <summary>
    ///     Construct a StreamSink that uses the last value if <see cref="Send" /> is called more than once per transaction.
    /// </summary>
    convenience override init()
    {
        self.init(fold: { (left, right) in fatalError("Send was called more than once in a transaction, which isn't allowed.  To combine the streams, pass a coalescing function to the StreamSink constructor.")
    
            return right
        })
    }

    /// <summary>
    ///     Construct a StreamSink that uses
    ///     <param name="coalesce" />
    ///     to combine values if <see cref="Send" /> is called more than once per transaction.
    /// </summary>
    /// <param name="coalesce">Function to combine values when <see cref="Send" /> is called more than once per transaction.</param>
    init(fold: (T,T) -> T)
    {
        self.fold = fold
        super.init()
        //self.coalescer = CoalesceHandler.Create(coalesce, out: self)
    }

    /// <summary>
    ///     Send a value.  This method may not be called from inside handlers registered with
    ///     <see cref="Stream{T}.Listen(Action{T})" /> or <see cref="Cell{T}.Listen(Action{T})" />.
    ///     An exception will be thrown, because sinks are for interfacing I/O to FRP only.  They are not meant to be used to
    ///     define new primitives.
    /// </summary>
    /// <param name="a">The value to send.</param>
    public func send(a: T) {
        Transaction.run(
        {
            trans in
            if (Transaction.inCallback > 0)
            {
                fatalError("Send() may not be called inside a Sodium callback.")
            }
            CoalesceHandler.create(self.fold, out: self)(trans, a)
        })
    }
}
