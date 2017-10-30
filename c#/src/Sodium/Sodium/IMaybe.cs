using System;

namespace Sodium
{
    public interface IMaybe
    {
        T Match<T>(Func<object, T> onSome, Func<T> onNone);
    }
}