using System;
using System.Collections.Generic;

namespace Sodium
{
    public static class Maybe
    {
        public static IMaybe<T> Just<T>(T value)
        {
            return new JustClass<T>(value);
        }

        public static IMaybe<T> Nothing<T>()
        {
            return NothingClass<T>.Value;
        }

        private class JustClass<T> : IMaybe<T>
        {
            private readonly T value;

            internal JustClass(T value)
            {
                this.value = value;
            }

            public override string ToString()
            {
                return "Just: " + this.value;
            }

            public TResult Accept<TResult>(IVisitor<TResult> visitor)
            {
                return visitor.Visit(this);
            }

            void IMaybe<T>.Match(Action<T> hasValueAction, Action nothingAction)
            {
                hasValueAction(this.value);
            }

            TResult IMaybe<T>.Match<TResult>(Func<T, TResult> hasValueFunc, Func<TResult> nothingFunc)
            {
                return hasValueFunc(this.value);
            }

            private bool Equals(JustClass<T> other)
            {
                return EqualityComparer<T>.Default.Equals(this.value, other.value);
            }

            public override bool Equals(object obj)
            {
                if (ReferenceEquals(null, obj))
                {
                    return false;
                }

                if (ReferenceEquals(this, obj))
                {
                    return true;
                }

                if (obj.GetType() != this.GetType())
                {
                    return false;
                }

                return this.Equals((JustClass<T>)obj);
            }

            public override int GetHashCode()
            {
                return EqualityComparer<T>.Default.GetHashCode(this.value);
            }
        }

        private class NothingClass<T> : IMaybe<T>
        {
            public static readonly NothingClass<T> Value = new NothingClass<T>();

            private NothingClass()
            {
            }

            public override string ToString()
            {
                return "Nothing";
            }

            public TResult Accept<TResult>(IVisitor<TResult> visitor)
            {
                return visitor.Visit(this);
            }

            void IMaybe<T>.Match(Action<T> hasValueAction, Action nothingAction)
            {
                nothingAction();
            }

            TResult IMaybe<T>.Match<TResult>(Func<T, TResult> hasValueFunc, Func<TResult> nothingFunc)
            {
                return nothingFunc();
            }

            public override bool Equals(object obj)
            {
                if (ReferenceEquals(null, obj))
                {
                    return false;
                }

                if (ReferenceEquals(this, obj))
                {
                    return true;
                }

                return obj.GetType() == this.GetType();
            }

            public override int GetHashCode()
            {
                return 1;
            }
        }

        public interface IVisitor<out TResult>
        {
            TResult Visit<T>(IMaybe<T> value);
        }
    }
}