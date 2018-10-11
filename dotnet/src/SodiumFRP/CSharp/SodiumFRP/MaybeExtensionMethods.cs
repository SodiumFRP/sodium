using System.Collections.Generic;
using System.Linq;

namespace SodiumFRP
{
    public static class MaybeExtensionMethods
    {
        public static Maybe<T> Flatten<T>(this Maybe<Maybe<T>> a) => a.Bind(v => v);

        public static IEnumerable<T> WhereMaybe<T>(this IEnumerable<Maybe<T>> o) =>
            (o ?? new Maybe<T>[0])
            .Select(m => m.Match(v => (Value: v, HasValue: true), () => (Value: default(T), HasValue: false)))
            .Where(p => p.HasValue)
            .Select(p => p.Value);

        public static Maybe<IEnumerable<T>> AllMaybeOrNone<T>(this IEnumerable<Maybe<T>> o)
        {
            (T Value, bool HasValue)[] rr = (o ?? new Maybe<T>[0])
                .Select(m => m.Match(v => (Value: v, HasValue: true), () => (Value: default(T), HasValue: false)))
                .ToArray();

            return rr.Any(r => !r.HasValue) ? Maybe.None : Maybe.Some(rr.Select(r => r.Value));
        }
    }
}