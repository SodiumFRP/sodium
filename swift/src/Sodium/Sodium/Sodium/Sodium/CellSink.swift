/// <summary>
///     A cell that allows values to be pushed into it, acting as an interface between the world of I/O and the world of
///     FRP.  Code that exports instances of <see cref="CellSink{T}" /> for read-only use should downcast to
///     <see cref="Cell{T}" />.
/// </summary>
/// <typeparam name="T">The type of values in the cell.</typeparam>
public class CellSink<T> : Cell<T>
{
    private let streamSink: StreamSink<T>

    /// <summary>
    ///     Construct a writable cell that uses the last value if <see cref="Send" /> is called more than once per transaction.
    /// </summary>
    /// <param name="initialValue">The initial value of the cell.</param>
    convenience init(_ initialValue: T)
    {
        self.init(streamSink: StreamSink<T>(), initialValue: initialValue)
    }

    /// <summary>
    ///     Construct a writable cell that uses
    ///     <param name="coalesce" />
    ///     to combine values if <see cref="Send" /> is called more than once per transaction.
    /// </summary>
    /// <param name="initialValue">The initial value of the cell.</param>
    /// <param name="coalesce">Function to combine values when <see cref="Send" /> is called more than once per transaction.</param>
    public convenience init(initialValue: T, coalesce: (T,T) -> T)
    {
        self.init(streamSink: StreamSink<T>(fold: coalesce), initialValue: initialValue)
    }

    private init(streamSink: StreamSink<T>, initialValue: T) {
        self.streamSink = streamSink
        super.init(stream: streamSink, initialValue: initialValue)
    }

    /// <summary>
    ///     Send a value, modifying the value of the cell.  This method may not be called from inside handlers registered with
    ///     <see cref="Stream{T}.Listen(Action{T})" /> or <see cref="Cell{T}.Listen(Action{T})" />.
    ///     An exception will be thrown, because sinks are for interfacing I/O to FRP only.  They are not meant to be used to
    ///     define new primitives.
    /// </summary>
    /// <param name="a">The value to send.</param>
    public func send(a: T) { self.streamSink.send(a) }
}
