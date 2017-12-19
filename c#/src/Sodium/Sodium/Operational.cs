using System.Collections.Generic;

namespace Sodium
{
    /// <summary>
    ///     Operational primitives that must be used with care.
    /// </summary>
    public static class Operational
    {
        /// <summary>
        ///     A stream that gives the updates/steps for a cell.
        /// </summary>
        /// <typeparam name="T">The type of the values in the cell.</typeparam>
        /// <param name="c"></param>
        /// <returns></returns>
        /// <remarks>
        ///     This is an OPERATIONAL primitive, which is not part of the main Sodium
        ///     API.  It breaks the property of non-detectability of cell steps/updates.
        ///     The rule with this primitive is that you should only use it in functions
        ///     that do not allow the caller to detect the cell updates.
        /// </remarks>
        public static Stream<T> Updates<T>(Cell<T> c) => Transaction.Apply(
            trans => c.Updates(trans).Coalesce(trans, (left, right) => right),
            false);

        /// <summary>
        ///     A stream that is guaranteed to fire once upon listening, giving the current
        ///     value of a cell, and thereafter gives the updates/steps for the cell.
        /// </summary>
        /// <typeparam name="T">The type of the values in the cell.</typeparam>
        /// <param name="c"></param>
        /// <returns></returns>
        /// <remarks>
        ///     This is an OPERATIONAL primitive, which is not part of the main Sodium
        ///     API.  It breaks the property of non-detectability of cell steps/updates.
        ///     The rule with this primitive is that you should only use it in functions
        ///     that do not allow the caller to detect the cell updates.
        /// </remarks>
        public static Stream<T> Value<T>(Cell<T> c) => Transaction.Apply(c.Value, false);

        /// <summary>
        ///     Push each stream event onto a new transaction guaranteed to come before the next externally
        ///     initiated transaction.  Same as <see cref="Split{T, TCollection}(Stream{TCollection})" /> but it works on a single
        ///     value.
        /// </summary>
        /// <typeparam name="T">The type of the stream to defer.</typeparam>
        /// <param name="s">The stream to defer.</param>
        /// <returns>A stream firing the deferred event firings.</returns>
        public static Stream<T> Defer<T>(Stream<T> s)
        {
            return Split<T, T[]>(s.Map(a => new[] { a }));
        }

        /// <summary>
        ///     Push each stream event in the list of streams onto a newly created transaction guaranteed
        ///     to come before the next externally initiated transaction.  Note that the semantics
        ///     are such that two different invocations of this method can put stream events into the same
        ///     new transaction, so the resulting stream's events could be simultaneous with
        ///     events output by <see cref="Split{T, TCollection}(Stream{TCollection})" /> or <see cref="Defer{T}(Stream{T})" />
        ///     invoked elsewhere in the code.
        /// </summary>
        /// <typeparam name="T">The collection item type of the stream to split.</typeparam>
        /// <typeparam name="TCollection">The collection type of the stream to split.</typeparam>
        /// <param name="s">The stream to split.</param>
        /// <returns>A stream firing the split event firings.</returns>
        public static Stream<T> Split<T, TCollection>(Stream<TCollection> s)
            where TCollection : IEnumerable<T>
        {
            Stream<T> @out = new Stream<T>(s.KeepListenersAlive);
            IListener l1 = s.Listen(
                new Node<T>(),
                (trans, aa) =>
                {
                    int childIx = 0;
                    foreach (T a in aa)
                    {
                        trans.Post(childIx, trans1 => @out.Send(trans1, a));
                        childIx++;
                    }
                });
            return @out.UnsafeAttachListener(l1);
        }
    }
}