using System;
using System.Runtime.CompilerServices;

namespace Sodium.Frp
{
    /// <summary>
    ///     A forward reference for a <see cref="Behavior{T}" /> equivalent to the <see cref="Behavior{T}" /> that is referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the behavior loop.</typeparam>
    public class BehaviorLoop<T> : LoopedBehavior<T>
    {
        private TransactionInternal transaction;

        private readonly object isLoopedLock = new object();
        private bool isLooped;

        [MethodImpl(MethodImplOptions.NoInlining)]
        public BehaviorLoop()
        {
            this.transaction = TransactionInternal.GetCurrentTransaction();

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
        [MethodImpl(MethodImplOptions.NoInlining)]
        public void Loop(Behavior<T> b) =>
            TransactionInternal.Apply(
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

                    return UnitInternal.Value;
                },
                false);
    }
}