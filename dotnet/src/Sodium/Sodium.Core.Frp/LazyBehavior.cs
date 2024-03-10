using System;

namespace Sodium.Frp
{
    internal class LazyBehavior<T> : Behavior<T>
    {
        internal Lazy<T> LazyInitialValue;

        internal LazyBehavior(TransactionInternal trans, Stream<T> stream, Lazy<T> lazyInitialValue)
            : base(stream, default(T))
        {
            this.LazyInitialValue = new Lazy<T>(() => GuardAgainstSend(trans, lazyInitialValue));

            trans.Sample(this.EnsureValueIsCreated);
        }

        private static T GuardAgainstSend(TransactionInternal trans, Lazy<T> v)
        {
            trans.InCallback++;
            try
            {
                // Don't allow transactions to interfere with Sodium
                // internals.
                return v.Value;
            }
            finally
            {
                trans.InCallback--;
            }
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
