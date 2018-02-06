using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public static class MaybeExtensionMethods
    {
        public static Maybe<T> Flatten<T>(this Maybe<Maybe<T>> a) => a.Bind(v => v);

        public static IEnumerable<T> WhereMaybe<T>(this IEnumerable<Maybe<T>> o) =>
            (o ?? new Maybe<T>[0])
            .Select(m => m.Match(v => (Value: v, HasValue: true), () => (Value: default(T), HasValue: false)))
            .Where(p => p.HasValue)
            .Select(p => p.Value);
    }
}