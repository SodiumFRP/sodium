using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public static class StreamExtensionMethods
    {
        /// <summary>
        ///     Merges a collection of streams and drops the stream's value specified earlier in the collection in the simultaneous
        ///     case.
        /// </summary>
        /// <param name="s">The collection of streams to merge.</param>
        /// <returns>
        ///     A stream that is the result of merging the collection of streams and dropping the stream's value specified
        ///     earlier in the collection in the simultaneous case.
        /// </returns>
        public static Stream<T> OrElse<T>(this IEnumerable<Stream<T>> s) => s.Merge((left, right) => left);

        /// <summary>
        ///     Merge a collection of streams of the same type into one, so that events on any input appear on the returned stream.
        /// </summary>
        /// <param name="s">The collection of streams to merge.</param>
        /// <param name="f">
        ///     Function to combine the values. It may construct FRP logic or use <see cref="Cell{T}.Sample" />.  Apart
        ///     from this the function must be pure.
        /// </param>
        /// <returns>
        ///     A stream which is the combination of event values from the collection of streams
        ///     <param name="s" />
        ///     .
        /// </returns>
        /// <remarks>
        ///     If the events are simultaneous (that is, one event from more than one stream
        ///     occurring in the same transaction), combine them into one using the specified combining function
        ///     so that the returned stream is guaranteed only ever to have one event per transaction.
        ///     The event from the stream earlier in the collection will appear at the left input of the combining function, and
        ///     the event from the stream later in the collection will appear at the right.
        /// </remarks>
        public static Stream<T> Merge<T>(this IEnumerable<Stream<T>> s, Func<T, T, T> f)
        {
            IReadOnlyList<Stream<T>> v = s.ToArray();
            return Transaction.Apply(trans => Merge(trans, v, 0, v.Count, f), false);
        }

        private static Stream<T> Merge<T>(
            Transaction trans,
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

        /// <summary>
        ///     Return a stream that only outputs events that have values, removing the <see cref="Maybe{T}" /> wrapper, and
        ///     discarding <see cref="Maybe.None" /> values.
        /// </summary>
        /// <param name="s">The stream of <see cref="Maybe{T}" /> values to filter.</param>
        /// <returns>
        ///     A stream that only outputs events that have values, removing the <see cref="Maybe{T}" /> wrapper, and
        ///     discarding <see cref="Maybe.None" /> values.
        /// </returns>
        public static Stream<T> FilterMaybe<T>(this Stream<Maybe<T>> s)
        {
            Stream<T> @out = new Stream<T>(s.KeepListenersAlive);
            IListener l = s.Listen(@out.Node, (trans2, a) => a.MatchSome(v => @out.Send(trans2, v)));
            return @out.UnsafeAttachListener(l);
        }
    }
}