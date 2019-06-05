using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Sodium.Frp
{
    public static class BehaviorExtensionMethods
    {
        /// <summary>
        ///     Sample the current value of the behavior.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <param name="b">The behavior.</param>
        /// <returns>The current value of the behavior.</returns>
        /// <remarks>
        ///     <para>
        ///         This method may be used inside the functions passed to primitives that apply them to streams,
        ///         including <see cref="StreamExtensionMethods.Map{T, TResult}(Stream{T}, Func{T, TResult})" /> in which case it is equivalent to
        ///         snapshotting the behavior,
        ///         <see cref=" StreamExtensionMethods.Snapshot{T, T2, TResult}(Stream{T}, Behavior{T2}, Func{T, T2, TResult})" />,
        ///         <see cref="StreamExtensionMethods.Filter{T}(Stream{T}, Func{T, bool})" />, and
        ///         <see cref="StreamExtensionMethods.Merge{T}(Stream{T}, Stream{T}, Func{T, T, T})" />
        ///     </para>
        ///     <para>
        ///         It can be best to use this method inside an explicit transaction (using
        ///         <see cref="Transaction.Run{T}(Func{T})" /> or <see cref="Transaction.RunVoid(Action)" />).
        ///         For example, a b.Sample() inside an explicit transaction along with a b.Updates().Listen(...) will capture the
        ///         current value and any updates without risk of missing any in between.
        ///     </para>
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static T Sample<T>(this Behavior<T> b) => b.SampleImpl();

        /// <summary>
        ///     Sample the current value of the behavior lazily.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <param name="b">The behavior.</param>
        /// <returns>A lazy which may be used to get the current value of the behavior.</returns>
        /// <remarks>
        ///     This is a variant of <see cref="Sample{T}" /> that works with the <see cref="BehaviorLoop{T}" /> class
        ///     when the behavior loop has not yet been looped.  It should be used in any code that is general
        ///     enough that it may be passed a <see cref="BehaviorLoop{T}" />.  See <see cref="StreamExtensionMethods.HoldLazy{T}(Stream{T}, Lazy{T})" />.
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Lazy<T> SampleLazy<T>(this Behavior<T> b) => b.SampleLazyImpl();

        /// <summary>
        ///     Transform the behavior values according to the supplied function, so the returned
        ///     behavior's values reflect the value of the function applied to the input behavior's values.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <typeparam name="TResult">The type of values fired by the returned behavior.</typeparam>
        /// <param name="b">The behavior.</param>
        /// <param name="f">
        ///     Function to apply to convert the values.  It must be a pure function.
        /// </param>
        /// <returns>An behavior which fires values transformed by <paramref name="f" /> for each value fired by this behavior.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<TResult> Map<T, TResult>(this Behavior<T> b, Func<T, TResult> f) =>
            b.MapImpl(f);

        /// <summary>
        ///     Lift a binary function into behaviors, so the returned behavior always reflects the specified function applied to the input
        ///     behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <typeparam name="T2">The type of second behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="b">The behavior.</param>
        /// <param name="b2">The second behavior.</param>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <returns>A behavior containing values resulting from the binary function applied to the input behaviors' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<TResult> Lift<T, T2, TResult>(this Behavior<T> b, Behavior<T2> b2, Func<T, T2, TResult> f) =>
            b.LiftImpl(b2, f);

        /// <summary>
        ///     Lift a ternary function into behaviors, so the returned behavior always reflects the specified function applied to the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <typeparam name="T2">The type of second behavior.</typeparam>
        /// <typeparam name="T3">The type of third behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="b">The behavior.</param>
        /// <param name="b2">The second behavior.</param>
        /// <param name="b3">The third behavior.</param>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <returns>A behavior containing values resulting from the ternary function applied to the input behaviors' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<TResult> Lift<T, T2, T3, TResult>(
            this Behavior<T> b,
            Behavior<T2> b2,
            Behavior<T3> b3,
            Func<T, T2, T3, TResult> f) => b.LiftImpl(b2, b3, f);

        /// <summary>
        ///     Lift a quaternary function into behaviors, so the returned behavior always reflects the specified function applied to the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <typeparam name="T2">The type of second behavior.</typeparam>
        /// <typeparam name="T3">The type of third behavior.</typeparam>
        /// <typeparam name="T4">The type of fourth behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="b">The behavior.</param>
        /// <param name="b2">The second behavior.</param>
        /// <param name="b3">The third behavior.</param>
        /// <param name="b4">The fourth behavior.</param>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <returns>A behavior containing values resulting from the quaternary function applied to the input behaviors' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<TResult> Lift<T, T2, T3, T4, TResult>(
            this Behavior<T> b,
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Func<T, T2, T3, T4, TResult> f) => b.LiftImpl(b2, b3, b4, f);

        /// <summary>
        ///     Lift a 5-argument function into behaviors, so the returned behavior always reflects the specified function applied to the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <typeparam name="T2">The type of second behavior.</typeparam>
        /// <typeparam name="T3">The type of third behavior.</typeparam>
        /// <typeparam name="T4">The type of fourth behavior.</typeparam>
        /// <typeparam name="T5">The type of fifth behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="b">The behavior.</param>
        /// <param name="b2">The second behavior.</param>
        /// <param name="b3">The third behavior.</param>
        /// <param name="b4">The fourth behavior.</param>
        /// <param name="b5">The fifth behavior.</param>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <returns>A behavior containing values resulting from the 5-argument function applied to the input behaviors' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<TResult> Lift<T, T2, T3, T4, T5, TResult>(
            this Behavior<T> b,
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Behavior<T5> b5,
            Func<T, T2, T3, T4, T5, TResult> f) => b.LiftImpl(b2, b3, b4, b5, f);

        /// <summary>
        ///     Lift a 6-argument function into behaviors, so the returned behavior always reflects the specified function applied to the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <typeparam name="T2">The type of second behavior.</typeparam>
        /// <typeparam name="T3">The type of third behavior.</typeparam>
        /// <typeparam name="T4">The type of fourth behavior.</typeparam>
        /// <typeparam name="T5">The type of fifth behavior.</typeparam>
        /// <typeparam name="T6">The type of sixth behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="b">The behavior.</param>
        /// <param name="b2">The second behavior.</param>
        /// <param name="b3">The third behavior.</param>
        /// <param name="b4">The fourth behavior.</param>
        /// <param name="b5">The fifth behavior.</param>
        /// <param name="b6">The sixth behavior.</param>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <returns>A behavior containing values resulting from the 6-argument function applied to the input behaviors' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<TResult> Lift<T, T2, T3, T4, T5, T6, TResult>(
            this Behavior<T> b,
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Behavior<T5> b5,
            Behavior<T6> b6,
            Func<T, T2, T3, T4, T5, T6, TResult> f) => b.LiftImpl(b2, b3, b4, b5, b6, f);

        /// <summary>
        ///     Apply a value inside a behavior to a function inside a behavior.  This is the primitive for all function lifting.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="b">The behavior.</param>
        /// <param name="bf">The behavior containing the function to apply the value to.</param>
        /// <returns>
        ///     A behavior whose value is the result of applying the current function in behavior <paramref name="bf" /> to this
        ///     behavior's current value.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<TResult> Apply<T, TResult>(this Behavior<T> b, Behavior<Func<T, TResult>> bf) =>
            b.ApplyImpl(bf);

        /// <summary>
        ///     Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <param name="bba">The behavior containing another behavior.</param>
        /// <returns>The unwrapped behavior.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<T> SwitchB<T>(this Behavior<Behavior<T>> bba) => bba.SwitchBImpl<T, Behavior<T>>();

        /// <summary>
        ///     Unwrap a cell inside a behavior to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="bca">The behavior containing a cell.</param>
        /// <returns>The unwrapped cell.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<T> SwitchC<T>(this Behavior<Cell<T>> bca) => bca.SwitchCImpl<T, Cell<T>>();

        /// <summary>
        ///     Unwrap a stream inside a behavior to give a time-varying stream implementation.
        ///     When the behavior changes value, the output stream will fire the simultaneous firing (if one exists) from the stream
        ///     which the behavior held at the beginning of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="bsa">The behavior containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> SwitchS<T>(this Behavior<Stream<T>> bsa) => bsa.SwitchSImpl<T, Stream<T>>();

        /// <summary>
        ///     Lift a function into an enumerable of behaviors, so the returned behavior always reflects the specified function applied to
        ///     the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behaviors.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="b">The enumerable of behaviors.</param>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <returns>A behavior containing values resulting from the function applied to the input behaviors' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<TResult> Lift<T, TResult>(
            this IEnumerable<Behavior<T>> b,
            Func<IReadOnlyList<T>, TResult> f) =>
            b.LiftBehaviorsImpl(f);

        /// <summary>
        ///     Lift a function into a collection of behaviors, so the returned behavior always reflects the specified function applied to
        ///     the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behaviors.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="b">The collection of behaviors.</param>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <returns>A behavior containing values resulting from the function applied to the input behaviors' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<TResult> Lift<T, TResult>(
            this IReadOnlyCollection<Behavior<T>> b,
            Func<IReadOnlyList<T>, TResult> f) => b.LiftBehaviorsImpl(f);

        /// <summary>
        ///     Lift into an enumerable of behaviors, so the returned behavior always reflects a list of the input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behaviors.</typeparam>
        /// <param name="b">The enumerable of behaviors.</param>
        /// <returns>A behavior containing a list of the input behaviors' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<IReadOnlyList<T>> Lift<T>(this IEnumerable<Behavior<T>> b) =>
            b.LiftBehaviorsImpl<T, Behavior<T>, IReadOnlyList<T>>(v => v);

        /// <summary>
        ///     Lift into a collection of behaviors, so the returned behavior always reflects a list of the input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behaviors.</typeparam>
        /// <param name="b">The collection of behaviors.</param>
        /// <returns>A behavior containing a list of the input behaviors' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<IReadOnlyList<T>> Lift<T>(this IReadOnlyCollection<Behavior<T>> b) =>
            b.LiftBehaviorsImpl<T, Behavior<T>, IReadOnlyList<T>>(v => v);
    }
}