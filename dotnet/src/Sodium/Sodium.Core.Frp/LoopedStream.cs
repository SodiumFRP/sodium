using System;

namespace Sodium.Frp
{
    /// <summary>
    ///     A forward reference for a <see cref="Stream{T}" /> equivalent to the <see cref="Stream{T}" /> that is referenced.
    /// </summary>
    /// <typeparam name="T">The type of values fired by the stream loop.</typeparam>
    public class LoopedStream<T> : Stream<T>
    {
        private readonly object isAssignedLock = new object();
        private bool isAssigned;

        internal LoopedStream()
        {
        }

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

        internal void Loop(TransactionInternal trans, Stream<T> stream)
        {
            lock (this.isAssignedLock)
            {
                if (this.isAssigned)
                {
                    throw new InvalidOperationException("Loop was looped more than once.");
                }

                this.isAssigned = true;
            }

            this.AttachListenerImpl(stream.Listen(this.Node, this.Send));

            lock (stream.KeepListenersAlive)
            {
                stream.KeepListenersAlive.Use(this.KeepListenersAlive);
            }
        }
    }
}