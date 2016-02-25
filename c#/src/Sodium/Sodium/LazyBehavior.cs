using System;

namespace Sodium
{
    public class LazyCell<T> : Cell<T>
    {
        internal Lazy<T> LazyInitialValue;

        internal LazyCell(Stream<T> stream, Lazy<T> lazyInitialValue)
            : base(stream, default(T))
        {
            this.LazyInitialValue = lazyInitialValue;
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