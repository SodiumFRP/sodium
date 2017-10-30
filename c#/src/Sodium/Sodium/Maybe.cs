using System;
using JetBrains.Annotations;

namespace Sodium
{
    public static class Maybe
    {
        public static Maybe<T> Some<T>(T value) => Maybe<T>.Some(value);
        public static readonly NoneType None = new NoneType();

        public struct NoneType
        {
        }
    }

    public struct Maybe<T>
    {
        private readonly bool hasValue;
        private readonly T value;

        private Maybe(T value)
        {
            if (value == null)
            {
                throw new ArgumentNullException(nameof(value), "Maybe.Some value cannot be null.");
            }

            this.hasValue = true;
            this.value = value;
        }

        public static Maybe<T> Some(T value) => new Maybe<T>(value);
        public static readonly Maybe<T> None = new Maybe<T>();

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

        public static implicit operator Maybe<T>(Maybe.NoneType _) => None;

        public override string ToString() => this.Match(v => $"{{Some: {v}}}", () => "{None}");
    }
}