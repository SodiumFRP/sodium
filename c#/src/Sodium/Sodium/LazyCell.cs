using System;

namespace Sodium
{
    public class LazyCell<T> : Cell<T>
    {
        internal Lazy<T> LazyInitialValue;

        internal LazyCell(Stream<T> stream, Lazy<T> lazyInitialValue)
            : base(stream, default(T)) => this.LazyInitialValue = lazyInitialValue;

        protected override void NotUsingInitialValue()
        {
            base.NotUsingInitialValue();

            this.LazyInitialValue = null;
        }

        internal override T SampleNoTransaction()
        {
            if (this.UsingInitialValue && this.LazyInitialValue != null)
            {
                this.ValueProperty = this.LazyInitialValue.Value;
                this.LazyInitialValue = null;
            }

            return this.ValueProperty;
        }
    }
}