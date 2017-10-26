using System;
using System.Collections.Generic;

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

        public TResult Match<TResult>(Func<T, TResult> hasValueFunc, Func<TResult> nothingFunc) => this.hasValue ? hasValueFunc(this.value) : nothingFunc();

        public static implicit operator Maybe<T>(Maybe.NoneType _) => new Maybe<T>();

        public override string ToString() => this.hasValue ? $"Some: {this.value}" : "None";

        private bool Equals(Maybe<T> other)
        {
            if (this.hasValue != other.hasValue)
            {
                return false;
            }

            if (this.hasValue)
            {
                return EqualityComparer<T>.Default.Equals(this.value, other.value);
            }

            return true;
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Maybe<T>))
            {
                return false;
            }

            return this.Equals((Maybe<T>)obj);
        }

        public override int GetHashCode()
        {
            return !this.hasValue ? 0 : EqualityComparer<T>.Default.GetHashCode(this.value);
        }
    }
}