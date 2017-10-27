using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;

namespace Sodium
{
    public static class Maybe
    {
        public static Maybe<T> Some<T>(T value) => new Maybe<T>(value);
        public static NoneType None = NoneType.Value;

        public struct NoneType
        {
            internal static readonly NoneType Value = new NoneType();
        }
    }

    public struct Maybe<T>
    {
        private readonly bool hasValue;
        private readonly T value;

        internal Maybe(T value)
        {
            if (value == null)
            {
                throw new ArgumentNullException(nameof(value), "Maybe.Some value cannot be null.");
            }

            this.hasValue = true;
            this.value = value;
        }

        // ReSharper disable once PureAttributeOnVoidMethod
        [Pure]
        public void Match(Action<T> hasValueAction, Action nothingAction)
        {
            if (this.hasValue)
            {
                hasValueAction(this.value);
            }
            else
            {
                nothingAction();
            }
        }

        [Pure]
        public TResult Match<TResult>(Func<T, TResult> hasValueFunc, Func<TResult> nothingFunc) => this.hasValue ? hasValueFunc(this.value) : nothingFunc();

        public static implicit operator Maybe<T>(Maybe.NoneType _) => new Maybe<T>();

        public override string ToString() => this.Match(v => $"Some: {v}", () => "None");
    }
}