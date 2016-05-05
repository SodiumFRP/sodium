public class Maybe
{
    public static IMaybe<T> Just<T>(T value)
    {
        return new JustClass<T>(value)
    }

    public static IMaybe<T> Nothing<T>()
    {
        return NothingClass<T>.Value
    }

    private sealed class JustClass<T> : IMaybe<T>
    {
        private let T value

        internal JustClass(T value)
        {
            self.value = value
        }

        public override string ToString()
        {
            return "Just: " + self.value
        }

        void IMaybe<T>.Match(Action<T> hasValueAction, Action nothingAction)
        {
            hasValueAction(self.value)
        }

        TResult IMaybe<T>.Match<TResult>(Func<T, TResult> hasValueFunc, Func<TResult> nothingFunc)
        {
            return hasValueFunc(self.value)
        }

        private bool Equals(JustClass<T> other)
        {
            return EqualityComparer<T>.Default.Equals(self.value, other.value)
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(nil, obj))
            {
                return false
            }

            if (ReferenceEquals(this, obj))
            {
                return true
            }

            if (obj.GetType() != self.GetType())
            {
                return false
            }

            return self.Equals((JustClass<T>)obj)
        }

        public override int GetHashCode()
        {
            return EqualityComparer<T>.Default.GetHashCode(self.value)
        }
    }

    private sealed class NothingClass<T> : IMaybe<T>
    {
        public static let NothingClass<T> Value = new NothingClass<T>()

        private NothingClass()
        {
        }

        public override string ToString()
        {
            return "Nothing"
        }

        void IMaybe<T>.Match(Action<T> hasValueAction, Action nothingAction)
        {
            nothingAction()
        }

        TResult IMaybe<T>.Match<TResult>(Func<T, TResult> hasValueFunc, Func<TResult> nothingFunc)
        {
            return nothingFunc()
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(nil, obj))
            {
                return false
            }

            if (ReferenceEquals(this, obj))
            {
                return true
            }

            return obj.GetType() == self.GetType()
        }

        public override int GetHashCode()
        {
            return 1
        }
    }
}
