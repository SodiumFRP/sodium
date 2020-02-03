using System.Collections.Generic;
using System.Linq;

namespace Sodium.Functional
{
    public static class MaybeExtensionMethods
    {
        [JetBrains.Annotations.Pure]
        public static Maybe<T> Flatten<T>(this Maybe<Maybe<T>> a) => a.Bind(v => v);

        [JetBrains.Annotations.Pure]
        public static IEnumerable<T> WhereMaybe<T>(this IEnumerable<Maybe<T>> o) =>
            (o ?? new Maybe<T>[0])
            .Select(m => m.Match(v => new ValueAndHasValue<T>(v, true), () => new ValueAndHasValue<T>(default(T), false)))
            .Where(p => p.HasValue)
            .Select(p => p.Value);

        [JetBrains.Annotations.Pure]
        public static Maybe<IEnumerable<T>> AllMaybeOrNone<T>(this IEnumerable<Maybe<T>> o)
        {
            ValueAndHasValue<T>[] rr = (o ?? new Maybe<T>[0])
                .Select(m => m.Match(v => new ValueAndHasValue<T>(v, true), () => new ValueAndHasValue<T>(default(T), false)))
                .ToArray();

            return rr.Any(r => !r.HasValue) ? Maybe.None : Maybe.Some(rr.Select(r => r.Value));
        }

        private struct ValueAndHasValue<T>
        {
            public ValueAndHasValue(T value, bool hasValue)
            {
                this.Value = value;
                this.HasValue = hasValue;
            }

            public T Value { get; }
            public bool HasValue { get; }
        }
    }
}