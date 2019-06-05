using System;
using System.Collections.Generic;

namespace Sodium.Frp
{
    internal static class MaybeInternal
    {
        public static MaybeInternal<T> Some<T>(T value) => MaybeInternal<T>.Some(value);
        public static readonly NoneType None = new NoneType();

        internal struct NoneType
        {
        }
    }

    internal struct MaybeInternal<T>
    {
        private readonly bool hasValue;
        private readonly T value;

        private MaybeInternal(T value)
        {
            this.hasValue = true;
            this.value = value;
        }

        #region Type Constructors

        internal static MaybeInternal<T> Some(T value) => new MaybeInternal<T>(value);
        internal static readonly MaybeInternal<T> None = new MaybeInternal<T>();

        #endregion

        #region Base Functionality

        internal TResult Match<TResult>(Func<T, TResult> onSome, Func<TResult> onNone) =>
            this.hasValue ? onSome(this.value) : onNone();

        internal void MatchVoid(Action<T> onSome, Action onNone)
        {
            if (this.hasValue)
            {
                onSome(this.value);
            }
            else
            {
                onNone();
            }
        }

        internal void MatchSome(Action<T> onSome)
        {
            if (this.hasValue)
            {
                onSome(this.value);
            }
        }

        internal void MatchNone(Action onNone)
        {
            if (!this.hasValue)
            {
                onNone();
            }
        }

        #endregion

        #region Helper Methods

        internal bool HasValue() => this.Match(v => true, () => false);

        #endregion

        public static implicit operator MaybeInternal<T>(MaybeInternal.NoneType _) => None;

        public static bool operator ==(MaybeInternal<T> x, MaybeInternal<T> y) =>
            x.hasValue == y.hasValue && EqualityComparer<T>.Default.Equals(x.value, y.value);

        public static bool operator !=(MaybeInternal<T> x, MaybeInternal<T> y) => !(x == y);
        public override bool Equals(object obj) => obj is MaybeInternal<T> m && this == m;

        public override int GetHashCode() =>
            (this.hasValue.GetHashCode() * 397) ^ EqualityComparer<T>.Default.GetHashCode(this.value);

        public override string ToString() => this.Match(v => $"{{Some: {v}}}", () => "{None}");
    }
}