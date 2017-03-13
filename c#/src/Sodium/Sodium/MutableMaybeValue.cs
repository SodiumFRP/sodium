using System;

namespace Sodium
{
    public class MutableMaybeValue<T> : IMaybe<T>
    {
        private IMaybe<T> value = Maybe.Nothing<T>();

        public void Set(T value) => this.value = Maybe.Just(value);
        public void Reset() => this.value = Maybe.Nothing<T>();
        public void Match(Action<T> hasValueAction, Action nothingAction) => this.value.Match(hasValueAction, nothingAction);
        public TResult Match<TResult>(Func<T, TResult> hasValueFunc, Func<TResult> nothingFunc) => this.value.Match(hasValueFunc, nothingFunc);
    }
}