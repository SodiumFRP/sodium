using System;

namespace Sodium.Frp
{
    /// <summary>
    ///     A stream that allows values to be pushed into it, acting as an interface between the world of I/O and the world of
    ///     FRP.  Code that exports StreamSinks for read-only use should downcast to <see cref="Stream{T}" />.
    /// </summary>
    /// <typeparam name="T">The type of values fired by the stream sink.</typeparam>
    public class StreamSink<T> : Stream<T>
    {
        private readonly Action<TransactionInternal, T> coalescer;

        internal StreamSink()
            : this(
                (left, right) => throw new InvalidOperationException(
                    "Send was called more than once in a transaction, which isn't allowed.  To combine the streams, pass a coalescing function to the StreamSink constructor."))
        {
        }

        internal StreamSink(Func<T, T, T> coalesce) => this.coalescer = CoalesceHandler.Create(coalesce, this);

        internal void SendImpl(T a)
        {
            TransactionInternal.Apply(
                (trans, _) =>
                {
                    if (TransactionInternal.InCallback > 0)
                    {
                        throw new InvalidOperationException("Send may not be called inside a Sodium callback.");
                    }

                    trans.Send(t => this.coalescer(t, a));

                    return UnitInternal.Value;
                },
                true);
        }
    }
}