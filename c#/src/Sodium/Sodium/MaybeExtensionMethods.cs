using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public static class MaybeExtensionMethods
    {
        /// <summary>
        ///     Map an <see cref="Maybe{T}" /> value using a mapping function if a value exists, or propogate the nothing value if
        ///     it does not.
        /// </summary>
        /// <param name="a">The <see cref="Maybe{T}" /> value to transform.</param>
        /// <param name="f">The function to transform the <see cref="Maybe{T}" /> input value <paramref name="a" />.</param>
        /// <typeparam name="T">The type of the maybe input value.</typeparam>
        /// <typeparam name="TResult">The type of the maybe result value.</typeparam>
        /// <returns>
        ///     The <see cref="Maybe{TResult}" /> which results from transforming <paramref name="a" /> using <paramref name="f" />
        ///     .
        /// </returns>
        public static Maybe<TResult> Map<T, TResult>(this Maybe<T> a, Func<T, TResult> f) => a.Bind(v => Maybe.Some(f(v)));

        public static Maybe<T> Flatten<T>(this Maybe<Maybe<T>> a) => a.Bind(v => v);

        public static bool HasValue<T>(this Maybe<T> o) => o.Match(v => true, () => false);

        public static IEnumerable<T> WhereMaybe<T>(this IEnumerable<Maybe<T>> o) => (o ?? new Maybe<T>[0])
            .Select(m => m.Match(v => (Value: v, HasValue: true), () => (Value: default(T), HasValue: false)))
            .Where(p => p.HasValue)
            .Select(p => p.Value);
    }
}