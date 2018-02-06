using System;

namespace Sodium
{
    /// <summary>
    ///     A forward reference for a <see cref="Behavior{T}" /> equivalent to the <see cref="Behavior{T}" /> that is referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the behavior loop.</typeparam>
    public class BehaviorLoop<T> : LazyBehavior<T>
    {
        private readonly StreamLoop<T> streamLoop;

        public BehaviorLoop()
            : this(new StreamLoop<T>())
        {
        }

        private BehaviorLoop(StreamLoop<T> streamLoop)
            : base(streamLoop, null) => this.streamLoop = streamLoop;

        /// <summary>
        ///     Resolve the loop to specify what the <see cref="BehaviorLoop{T}" /> was a forward reference to.  This method
        ///     must be called inside the same transaction as the one in which this <see cref="BehaviorLoop{T}" /> instance was
        ///     created and used.
        ///     This requires an explicit transaction to be created with <see cref="Transaction.Run{T}(Func{T})" /> or
        ///     <see cref="Transaction.RunVoid(Action)" />.
        /// </summary>
        /// <param name="b">The behavior that was forward referenced.</param>
        public void Loop(Behavior<T> b)
        {
            Transaction.Apply(
                trans =>
                {
                    this.streamLoop.Loop(b.Updates(trans));
                    this.LazyInitialValue = b.SampleLazy(trans);
                    return Unit.Value;
                },
                false);
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