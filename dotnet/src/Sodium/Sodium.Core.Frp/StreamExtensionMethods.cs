using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium.Frp
{
    internal static class StreamExtensionMethodsInternal
    {
        internal static Stream<T> OrElseImpl<T, T2>(this IEnumerable<T2> s) where T2 : Stream<T> => s.MergeImpl<T, T2>((left, right) => left);

        internal static Stream<T> MergeImpl<T, T2>(this IEnumerable<T2> s, Func<T, T, T> f) where T2 : Stream<T>
        {
            IReadOnlyList<Stream<T>> v = s.ToArray();
            return TransactionInternal.Apply((trans, _) => Merge(trans, v, 0, v.Count, f), false);
        }

        private static Stream<T> Merge<T>(
            TransactionInternal trans,
            IReadOnlyList<Stream<T>> e,
            int start,
            int end,
            Func<T, T, T> f)
        {
            int n = end - start;

            if (n == 0)
            {
                return new Stream<T>();
            }

            if (n == 1)
            {
                return e[start];
            }

            if (n == 2)
            {
                return e[start].Merge(trans, e[start + 1], f);
            }

            int mid = (start + end) / 2;
            return Merge(trans, e, start, mid, f).Merge(trans, Merge(trans, e, mid, end, f), f);
        }

        internal static Stream<T> FilterMaybeImpl<T, TMaybe>(this Stream<TMaybe> s, Action<TMaybe, Action<T>> matchSome)
        {
            Stream<T> @out = new Stream<T>(s.KeepListenersAlive);

            IListener l =
                s.Listen(
                    @out.Node,
                    (trans2, a) => matchSome(a, v => @out.Send(trans2, v)));

            return @out.UnsafeAttachListener(l);
        }

        internal static Stream<T> FilterMaybeInternal<T>(this Stream<MaybeInternal<T>> s) =>
            s.FilterMaybeImpl<T, MaybeInternal<T>>((m, a) => m.MatchSome(a));
    }
}