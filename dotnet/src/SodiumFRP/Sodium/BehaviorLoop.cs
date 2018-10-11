using System;

namespace Sodium
{
    /// <summary>
    ///     A forward reference for a <see cref="Behavior{T}" /> equivalent to the <see cref="Behavior{T}" /> that is referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the behavior loop.</typeparam>
    public class BehaviorLoop<T> : LoopedBehavior<T>
    {
        private Transaction transaction;

        private readonly object isLoopedLock = new object();
        private bool isLooped;

        public BehaviorLoop()
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
        ///     Resolve the loop to specify what the <see cref="BehaviorLoop{T}" /> was a forward reference to.  This method
        ///     must be called inside the same transaction as the one in which this <see cref="BehaviorLoop{T}" /> instance was
        ///     created and used.
        ///     This requires an explicit transaction to be created with <see cref="Transaction.Run{T}(Func{T})" /> or
        ///     <see cref="Transaction.RunVoid(Action)" />.
        /// </summary>
        /// <param name="b">The behavior that was forward referenced.</param>
        public void Loop(Behavior<T> b) =>
            Transaction.Apply(
                (trans, _) =>
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

                        throw new InvalidOperationException(
                            "Loop must be looped in the same transaction that it was created in.");
                    }

                    this.transaction = null;

                    this.Loop(trans, b);

                    return Unit.Value;
                },
                false);
    }

    /// <summary>
    ///     A forward reference for a <see cref="Behavior{T}" /> equivalent to the <see cref="Behavior{T}" /> that is referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the behavior loop.</typeparam>
    public class LoopedBehavior<T> : LazyBehavior<T>
    {
        private readonly LoopedStream<T> streamLoop;

        internal LoopedBehavior()
            : this(new LoopedStream<T>())
        {
        }

        private LoopedBehavior(LoopedStream<T> streamLoop)
            : base(streamLoop) => this.streamLoop = streamLoop;

        protected internal void Loop(Transaction trans, Behavior<T> b)
        {
            this.streamLoop.Loop(trans, b.Updates());
            this.LazyInitialValue = b.SampleLazy(trans);
        }

        /// <summary>
        ///     Return a reference to this <see cref="BehaviorLoop{T}" /> as a <see cref="Behavior{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="BehaviorLoop{T}" /> as a <see cref="Behavior{T}" />.</returns>
        public Behavior<T> AsBehavior() => this;

        internal override T SampleNoTransaction()
        {
            if (!this.streamLoop.IsAssigned)
            {
                throw new InvalidOperationException("BehaviorLoop was sampled before it was looped.");
            }

            return base.SampleNoTransaction();
        }
    }
}