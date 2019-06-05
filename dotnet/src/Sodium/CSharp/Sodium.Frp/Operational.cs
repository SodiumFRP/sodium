using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Sodium.Frp
{
    /// <summary>
    ///     Operational primitives that must be used with care.
    /// </summary>
    public static class Operational
    {
        /// <summary>
        ///     A stream that gives the updates/steps for a behavior.
        /// </summary>
        /// <typeparam name="T">The type of the values in the behavior.</typeparam>
        /// <param name="b"></param>
        /// <returns></returns>
        /// <remarks>
        ///     This is an OPERATIONAL primitive, which is not part of the main Sodium
        ///     API.  It breaks the property of non-detectability of behavior steps/updates.
        ///     The rule with this primitive is that you should only use it in functions
        ///     that do not allow the caller to detect the behavior updates.
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Updates<T>(Behavior<T> b) => OperationalInternal.UpdatesImpl(b);

        /// <summary>
        ///     A stream that is guaranteed to fire once upon listening, giving the current
        ///     value of a behavior, and thereafter gives the updates/steps for the behavior.
        /// </summary>
        /// <typeparam name="T">The type of the values in the behavior.</typeparam>
        /// <param name="b"></param>
        /// <returns></returns>
        /// <remarks>
        ///     This is an OPERATIONAL primitive, which is not part of the main Sodium
        ///     API.  It breaks the property of non-detectability of behavior steps/updates.
        ///     The rule with this primitive is that you should only use it in functions
        ///     that do not allow the caller to detect the behavior updates.
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Value<T>(Behavior<T> b) => OperationalInternal.ValueImpl(b);

        /// <summary>
        ///     Push each stream event onto a new transaction guaranteed to come before the next externally
        ///     initiated transaction.  Same as <see cref="Split{T, TCollection}(Stream{TCollection})" /> but it works on a single
        ///     value.
        /// </summary>
        /// <typeparam name="T">The type of the stream to defer.</typeparam>
        /// <param name="s">The stream to defer.</param>
        /// <returns>A stream firing the deferred event firings.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Defer<T>(Stream<T> s) => OperationalInternal.DeferImpl(s);

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
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Split<T, TCollection>(Stream<TCollection> s)
            where TCollection : IEnumerable<T> =>
            OperationalInternal.SplitImpl<T, TCollection>(s);
    }
}