using System;

namespace Sodium
{
    public class LazyBehavior<T> : Behavior<T>
    {
        internal Lazy<T> LazyInitialValue;

        internal LazyBehavior(Stream<T> stream)
            : base(stream, default(T))
        {
        }

        internal LazyBehavior(Transaction trans, Stream<T> stream, Lazy<T> lazyInitialValue)
            : base(stream, default(T))
        {
            this.LazyInitialValue = lazyInitialValue;

            trans.Sample(this.EnsureValueIsCreated);
        }

        protected override void NotUsingInitialValue()
        {
            base.NotUsingInitialValue();

            this.LazyInitialValue = null;
        }

        internal override T SampleNoTransaction()
        {
            this.EnsureValueIsCreated();

            return this.ValueProperty;
        }

        private void EnsureValueIsCreated()
        {
            if (this.UsingInitialValue && this.LazyInitialValue != null)
            {
                this.ValueProperty = this.LazyInitialValue.Value;
                this.LazyInitialValue = null;
            }
        }
    }
}