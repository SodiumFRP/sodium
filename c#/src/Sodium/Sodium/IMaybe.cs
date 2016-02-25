using System;

namespace Sodium
{
    public interface IMaybe<out T>
    {
        void Match(Action<T> hasValueAction, Action nothingAction);
        TResult Match<TResult>(Func<T, TResult> hasValueFunc, Func<TResult> nothingFunc);
    }
}