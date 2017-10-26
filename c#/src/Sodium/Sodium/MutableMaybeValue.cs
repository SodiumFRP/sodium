using System;

namespace Sodium
{
    public class MutableMaybeValue<T>
    {
        public Maybe<T> Value { get; private set; }

        public void Set(T value) => this.Value = Maybe.Some(value);
        public void Reset() => this.Value = Maybe.None;

        public void Match(Action<T> hasValueAction, Action nothingAction) => this.Value.Match(hasValueAction, nothingAction);
        public TResult Match<TResult>(Func<T, TResult> hasValueFunc, Func<TResult> nothingFunc) => this.Value.Match(hasValueFunc, nothingFunc);
    }
}