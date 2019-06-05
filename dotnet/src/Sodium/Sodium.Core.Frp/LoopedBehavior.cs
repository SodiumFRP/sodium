using System;

namespace Sodium.Frp
{
    /// <summary>
    ///     A forward reference for a <see cref="Behavior{T}" /> equivalent to the <see cref="Behavior{T}" /> that is referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the behavior loop.</typeparam>
    public class LoopedBehavior<T> : Behavior<T>
    {
        private readonly LoopedStream<T> streamLoop;

        private Lazy<T> lazyInitialValue;

        internal LoopedBehavior()
            : this(new LoopedStream<T>())
        {
        }

        private LoopedBehavior(LoopedStream<T> streamLoop)
            : base(streamLoop, default(T)) => this.streamLoop = streamLoop;

        internal void Loop(TransactionInternal trans, Behavior<T> b)
        {
            this.streamLoop.Loop(trans, b.Updates());
            this.lazyInitialValue = b.SampleLazy(trans);
        }

        protected override void NotUsingInitialValue()
        {
            base.NotUsingInitialValue();

            this.lazyInitialValue = null;
        }

        internal override T SampleNoTransaction()
        {
            if (!this.streamLoop.IsAssigned)
            {
                throw new InvalidOperationException("BehaviorLoop was sampled before it was looped.");
            }

            this.EnsureValueIsCreated();

            return this.ValueProperty;
        }

        private void EnsureValueIsCreated()
        {
            if (this.UsingInitialValue && this.lazyInitialValue != null)
            {
                this.ValueProperty = this.lazyInitialValue.Value;
                this.lazyInitialValue = null;
            }
        }
    }
}