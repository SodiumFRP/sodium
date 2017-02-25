using System;

namespace Sodium
{
    /// <summary>
    ///     A stream that allows values to be pushed into it, acting as an interface between the world of I/O and the world of
    ///     FRP.  Code that exports StreamSinks for read-only use should downcast to <see cref="Stream{T}" />.
    /// </summary>
    /// <typeparam name="T">The type of values fired by the stream.</typeparam>
    public class StreamSink<T> : Stream<T>
    {
        private readonly Action<Transaction, T> coalescer;

        /// <summary>
        ///     Construct a StreamSink that throws an exception if <see cref="Send" /> is called more than once per transaction.
        /// </summary>
        public StreamSink()
            : this((left, right) => { throw new InvalidOperationException("Send was called more than once in a transaction, which isn't allowed.  To combine the streams, pass a coalescing function to the StreamSink constructor."); })
        {
        }

        /// <summary>
        ///     Construct a StreamSink that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="Send" /> is called more than once per transaction.
        /// </summary>
        /// <param name="coalesce">Function to combine values when <see cref="Send" /> is called more than once per transaction.</param>
        public StreamSink(Func<T, T, T> coalesce)
        {
            this.coalescer = CoalesceHandler.Create(coalesce, this);
        }

        /// <summary>
        ///     Send a value.  This method may not be called from inside handlers registered with
        ///     <see cref="Stream{T}.Listen(Action{T})" /> or <see cref="Cell{T}.Listen(Action{T})" />.
        ///     An exception will be thrown, because sinks are for interfacing I/O to FRP only.  They are not meant to be used to
        ///     define new primitives.
        /// </summary>
        /// <param name="a">The value to send.</param>
        public void Send(T a)
        {
            Transaction.Run(trans =>
            {
                if (Transaction.InCallback > 0)
                {
                    throw new InvalidOperationException("Send() may not be called inside a Sodium callback.");
                }
                this.coalescer(trans, a);
            });
        }

        /// <summary>
        ///     Return a reference to this <see cref="StreamSink{T}" /> as a <see cref="Stream{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="StreamSink{T}" /> as a <see cref="Stream{T}" />.</returns>
        public Stream<T> AsStream() => this;
    }
}