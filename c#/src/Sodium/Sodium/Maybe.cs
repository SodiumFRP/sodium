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

    public struct Maybe<T> : IMaybe
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

        T1 IMaybe.Match<T1>(Func<object, T1> onSome, Func<T1> onNone) => this.Match(v => onSome(v), onNone);

        [Pure]
        public TResult Match<TResult>(Func<T, TResult> onSome, Func<TResult> onNone) => this.hasValue ? onSome(this.value) : onNone();

        public static implicit operator Maybe<T>(Maybe.NoneType _) => None;

        public override string ToString() => this.Match(v => $"{{Some: {v}}}", () => "{None}");
    }
}