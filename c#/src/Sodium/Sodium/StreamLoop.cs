using System;

namespace Sodium
{
    /// <summary>
    ///     A forward reference for a <see cref="Stream{T}" /> equivalent to the <see cref="Stream{T}" /> that is referenced.
    /// </summary>
    /// <typeparam name="T">The type of values fired by the stream loop.</typeparam>
    public class StreamLoop<T> : LoopedStream<T>
    {
        private Transaction transaction;

        private readonly object isLoopedLock = new object();
        private bool isLooped;

        public StreamLoop()
        {
            this.transaction = Transaction.GetCurrentTransaction();

            if (this.transaction == null)
            {
                throw new InvalidOperationException("Loop must be created within an explicit transaction.");
            }

            this.transaction.Last(
                () =>
                {
                    if (this.transaction != null)
                    {
                        this.transaction = null;

                        throw new InvalidOperationException("Loop was not looped.");
                    }
                });
        }

        /// <summary>
        ///     Resolve the loop to specify what the <see cref="StreamLoop{T}" /> was a forward reference to.  This method
        ///     must be called inside the same transaction as the one in which this <see cref="StreamLoop{T}" /> instance was
        ///     created and used.
        ///     This requires an explicit transaction to be created with <see cref="Transaction.Run{T}(Func{T})" /> or
        ///     <see cref="Transaction.RunVoid(Action)" />.
        /// </summary>
        /// <param name="stream">The stream that was forward referenced.</param>
        public void Loop(Stream<T> stream) =>
            Transaction.Apply(
                trans =>
                {
                    lock (this.isLoopedLock)
                    {
                        if (this.isLooped)
                        {
                            throw new InvalidOperationException("Loop was looped more than once.");
                        }

                        this.isLooped = true;
                    }

                    if (trans != this.transaction)
                    {
                        this.transaction = null;

                        throw new InvalidOperationException("Loop must be looped in the same transaction that it was created in.");
                    }

                    this.transaction = null;

                    this.Loop(trans, stream);

                    return Unit.Value;
                },
                false);
    }

    /// <summary>
    ///     A forward reference for a <see cref="Stream{T}" /> equivalent to the <see cref="Stream{T}" /> that is referenced.
    /// </summary>
    /// <typeparam name="T">The type of values fired by the stream loop.</typeparam>
    public class LoopedStream<T> : Stream<T>
    {
        private readonly object isAssignedLock = new object();
        private bool isAssigned;

        internal bool IsAssigned
        {
            get
            {
                lock (this.isAssignedLock)
                {
                    return this.isAssigned;
                }
            }
        }

        protected internal void Loop(Transaction trans, Stream<T> stream)
        {
            lock (this.isAssignedLock)
            {
                if (this.isAssigned)
                {
                    throw new InvalidOperationException("Loop was looped more than once.");
                }

                this.isAssigned = true;
            }

            this.AttachListener(stream.Listen(this.Node, this.Send));

            lock (stream.KeepListenersAlive)
            {
                stream.KeepListenersAlive.Use(this.KeepListenersAlive);
            }
        }

        /// <summary>
        ///     Return a reference to this <see cref="StreamLoop{T}" /> as a <see cref="Stream{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="StreamLoop{T}" /> as a <see cref="Stream{T}" />.</returns>
        public Stream<T> AsStream() => this;
    }
}