using System;

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
        }

        public interface IVisitor<out TResult>
        {
            TResult Visit<T>(IMaybe<T> value);
        }
    }
}