using System.Collections.Generic;

namespace Sodium
{
    public static class MaybeExtensionMethods
    {
        public static bool Equals<T>(this IMaybe<T> o, IMaybe<T> other)
        {
            return o.Match(x => other.Match(y => EqualityComparer<T>.Default.Equals(x, y), () => false), () => other.Match(_ => false, () => true));
        }
    }
}