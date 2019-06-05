using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Sodium.Frp
{
    public static class CellExtensionMethods
    {
        /// <summary>
        ///     Sample the current value of the cell.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <returns>The current value of the cell.</returns>
        /// <remarks>
        ///     <para>
        ///         This method may be used inside the functions passed to primitives that apply them to streams,
        ///         including <see cref="StreamExtensionMethods.Map{T, TResult}(Stream{T}, Func{T, TResult})" /> in which case it is equivalent to
        ///         snapshotting the cell,
        ///         <see cref=" StreamExtensionMethods.Snapshot{T, T2, TResult}(Stream{T}, Cell{T2}, Func{T, T2, TResult})" />,
        ///         <see cref="StreamExtensionMethods.Filter{T}(Stream{T}, Func{T, bool})" />, and
        ///         <see cref="StreamExtensionMethods.Merge{T}(Stream{T}, Stream{T}, Func{T, T, T})" />
        ///     </para>
        ///     <para>
        ///         It should generally be avoided in favor of <see cref="Listen{T}(Cell{T}, Action{T})" />
        ///         so updates aren't missed, but in many circumstances it makes sense.
        ///     </para>
        ///     <para>
        ///         It can be best to use this method inside an explicit transaction (using
        ///         <see cref="Transaction.Run{T}(Func{T})" /> or <see cref="Transaction.RunVoid(Action)" />).
        ///         For example, a c.Sample() inside an explicit transaction along with a c.Updates().Listen(...) will capture the
        ///         current value and any updates without risk of missing any in between.
        ///     </para>
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static T Sample<T>(this Cell<T> c) => c.SampleImpl();

        /// <summary>
        ///     Sample the current value of the cell lazily.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <returns>A lazy which may be used to get the current value of the cell.</returns>
        /// <remarks>
        ///     This is a variant of <see cref="Sample{T}" /> that works with the <see cref="CellLoop{T}" /> class
        ///     when the cell loop has not yet been looped.  It should be used in any code that is general
        ///     enough that it may be passed a <see cref="CellLoop{T}" />.  See <see cref="StreamExtensionMethods.HoldLazy{T}(Stream{T}, Lazy{T})" />.
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Lazy<T> SampleLazy<T>(this Cell<T> c) => c.SampleLazyImpl();

        /// <summary>
        ///     Gets the stream of discrete updates to this cell.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <returns>
        ///     The stream of discrete updates to this cell.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Updates<T>(this Cell<T> c) => c.UpdatesImpl;

        /// <summary>
        ///     Gets the stream of values of this cell.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <returns>
        ///     The stream of values of this cell.
        /// </returns>
        /// <remarks>
        ///     This stream is identical to the stream returned by <see cref="Updates{T}(Cell{T})" /> except that it also fires
        ///     during the transaction in which it was obtained.
        ///     To observe the first value, this property must be accessed and used within the same explicit transaction.
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Values<T>(this Cell<T> c) => c.ValuesImpl;

        /// <summary>
        ///     Return a reference to this <see cref="Cell{T}" /> as a <see cref="Behavior{T}" />.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <returns>A reference to this <see cref="Cell{T}" /> as a <see cref="Behavior{T}" />.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<T> AsBehavior<T>(this Cell<T> c) => c.BehaviorImpl;

        /// <summary>
        ///     Listen for updates to the value of this cell.  The returned <see cref="IListener" /> may be
        ///     disposed to stop listening.  This is an OPERATIONAL mechanism for interfacing between
        ///     the world of I/O and FRP.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="handler">The handler to execute for each value.</param>
        /// <returns>An <see cref="IListener" /> which may be disposed to stop listening.</returns>
        /// <remarks>
        ///     <para>
        ///         No assumptions should be made about what thread the handler is called on and it should not block.
        ///         Neither <see cref="StreamSink{T}.Send" /> nor <see cref="CellSinkExtensionMethods.Send{T}" /> may be called from the
        ///         handler.
        ///         They will throw an exception because this method is not meant to be used to create new primitives.
        ///     </para>
        ///     <para>
        ///         If the <see cref="IListener" /> is not disposed, it will continue to listen until this cell is either
        ///         disposed or garbage collected.
        ///     </para>
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IStrongListener Listen<T>(this Cell<T> c, Action<T> handler) => c.ListenImpl(handler);

        /// <summary>
        ///     Listen for updates to the value of this cell.  The returned <see cref="IListener" /> may be
        ///     disposed to stop listening, or it will automatically stop listening when it is garbage collected.
        ///     This is an OPERATIONAL mechanism for interfacing between the world of I/O and FRP.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="handler">The handler to execute for each value.</param>
        /// <returns>An <see cref="IListener" /> which may be disposed to stop listening.</returns>
        /// <remarks>
        ///     <para>
        ///         No assumptions should be made about what thread the handler is called on and it should not block.
        ///         Neither <see cref="StreamSink{T}.Send" /> nor <see cref="CellSinkExtensionMethods.Send{T}" /> may be called from the
        ///         handler.
        ///         They will throw an exception because this method is not meant to be used to create new primitives.
        ///     </para>
        ///     <para>
        ///         If the <see cref="IListener" /> is not disposed, it will continue to listen until this cell is either
        ///         disposed or garbage collected or the listener itself is garbage collected.
        ///     </para>
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IWeakListener ListenWeak<T>(this Cell<T> c, Action<T> handler) => c.ListenWeakImpl(handler);

        /// <summary>
        ///     Transform the cell values according to the supplied function, so the returned
        ///     cell's values reflect the value of the function applied to the input cell's values.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <typeparam name="TResult">The type of values fired by the returned cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="f">
        ///     Function to apply to convert the values.  It must be a pure function.
        /// </param>
        /// <returns>A cell which fires values transformed by <paramref name="f" /> for each value fired by this cell.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TResult> Map<T, TResult>(this Cell<T> c, Func<T, TResult> f) => c.MapImpl(f);

        /// <summary>
        ///     Lift a binary function into cells, so the returned cell always reflects the specified function applied to the input
        ///     cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="c2">The second cell.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the binary function applied to the input cells' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TResult> Lift<T, T2, TResult>(this Cell<T> c, Cell<T2> c2, Func<T, T2, TResult> f) =>
            c.LiftImpl(c2, f);

        /// <summary>
        ///     Lift a ternary function into cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="T3">The type of third cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="c2">The second cell.</param>
        /// <param name="c3">The third cell.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the ternary function applied to the input cells' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TResult> Lift<T, T2, T3, TResult>(
            this Cell<T> c,
            Cell<T2> c2,
            Cell<T3> c3,
            Func<T, T2, T3, TResult> f) => c.LiftImpl(c2, c3, f);

        /// <summary>
        ///     Lift a quaternary function into cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="T3">The type of third cell.</typeparam>
        /// <typeparam name="T4">The type of fourth cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="c2">The second cell.</param>
        /// <param name="c3">The third cell.</param>
        /// <param name="c4">The fourth cell.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the quaternary function applied to the input cells' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TResult> Lift<T, T2, T3, T4, TResult>(
            this Cell<T> c,
            Cell<T2> c2,
            Cell<T3> c3,
            Cell<T4> c4,
            Func<T, T2, T3, T4, TResult> f) => c.LiftImpl(c2, c3, c4, f);

        /// <summary>
        ///     Lift a 5-argument function into cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="T3">The type of third cell.</typeparam>
        /// <typeparam name="T4">The type of fourth cell.</typeparam>
        /// <typeparam name="T5">The type of fifth cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="c2">The second cell.</param>
        /// <param name="c3">The third cell.</param>
        /// <param name="c4">The fourth cell.</param>
        /// <param name="c5">The fifth cell.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the 5-argument function applied to the input cells' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TResult> Lift<T, T2, T3, T4, T5, TResult>(
            this Cell<T> c,
            Cell<T2> c2,
            Cell<T3> c3,
            Cell<T4> c4,
            Cell<T5> c5,
            Func<T, T2, T3, T4, T5, TResult> f) => c.LiftImpl(c2, c3, c4, c5, f);

        /// <summary>
        ///     Lift a 6-argument function into cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="T3">The type of third cell.</typeparam>
        /// <typeparam name="T4">The type of fourth cell.</typeparam>
        /// <typeparam name="T5">The type of fifth cell.</typeparam>
        /// <typeparam name="T6">The type of sixth cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="c2">The second cell.</param>
        /// <param name="c3">The third cell.</param>
        /// <param name="c4">The fourth cell.</param>
        /// <param name="c5">The fifth cell.</param>
        /// <param name="c6">The sixth cell.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the 6-argument function applied to the input cells' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TResult> Lift<T, T2, T3, T4, T5, T6, TResult>(
            this Cell<T> c,
            Cell<T2> c2,
            Cell<T3> c3,
            Cell<T4> c4,
            Cell<T5> c5,
            Cell<T6> c6,
            Func<T, T2, T3, T4, T5, T6, TResult> f) => c.LiftImpl(c2, c3, c4, c5, c6, f);

        /// <summary>
        ///     Apply a value inside a cell to a function inside a cell.  This is the primitive for all function lifting.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="cf">The cell containing the function to apply the value to.</param>
        /// <returns>
        ///     A cell whose value is the result of applying the current function in cell <paramref name="cf" /> to this
        ///     cell's current value.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TResult> Apply<T, TResult>(this Cell<T> c, Cell<Func<T, TResult>> cf) => c.ApplyImpl(cf);

        /// <summary>
        ///     Return a cell whose stream only receives events which have a different value than the previous event.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<T> Calm<T>(this Cell<T> c) => c.CalmImpl(EqualityComparer<T>.Default.Equals);

        /// <summary>
        ///     Return a cell whose stream only receives events which have a different value than the previous event.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="comparer">The equality comparer to use to determine if two items are equal.</param>
        /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<T> Calm<T>(this Cell<T> c, IEqualityComparer<T> comparer) => c.CalmImpl(comparer.Equals);

        /// <summary>
        ///     Return a cell whose stream only receives events which have a different value than the previous event.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="c">The cell.</param>
        /// <param name="areEqual">The function to use to determine if two items are equal.</param>
        /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<T> Calm<T>(this Cell<T> c, Func<T, T, bool> areEqual) => c.CalmImpl(areEqual);

        /// <summary>
        ///     Unwrap a behavior inside a cell to give a time-varying behavior implementation.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <param name="cba">The cell containing a behavior.</param>
        /// <returns>The unwrapped behavior.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<T> SwitchB<T>(this Cell<Behavior<T>> cba) => cba.SwitchBImpl<T, Behavior<T>>();

        /// <summary>
        ///     Unwrap a cell inside another cell to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="cca">The cell containing another cell.</param>
        /// <returns>The unwrapped cell.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<T> SwitchC<T>(this Cell<Cell<T>> cca) => cca.SwitchCImpl<T, Cell<T>>();

        /// <summary>
        ///     Unwrap a stream inside a cell to give a time-varying stream implementation.
        ///     When the cell changes value, the output stream will fire the simultaneous firing (if one exists) from the
        ///     stream which the cell held at the beginning of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="csa">The cell containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> SwitchS<T>(this Cell<Stream<T>> csa) => csa.SwitchSImpl<T, Stream<T>>();

        /// <summary>
        ///     Lift a function into an enumerable of cells, so the returned cell always reflects the specified function applied to
        ///     the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The enumerable of cells.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the function applied to the input cells' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TResult> Lift<T, TResult>(
            this IEnumerable<Cell<T>> c,
            Func<IReadOnlyList<T>, TResult> f) => c.LiftCellsImpl(f);

        /// <summary>
        ///     Lift a function into a collection of cells, so the returned cell always reflects the specified function applied to
        ///     the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The collection of cells.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the function applied to the input cells' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TResult> Lift<T, TResult>(
            this IReadOnlyCollection<Cell<T>> c,
            Func<IReadOnlyList<T>, TResult> f) => c.LiftCellsImpl(f);

        /// <summary>
        ///     Lift into an enumerable of cells, so the returned cell always reflects a list of the input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <param name="c">The enumerable of cells.</param>
        /// <returns>A cell containing a list of the input cells' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<IReadOnlyList<T>> Lift<T>(this IEnumerable<Cell<T>> c) =>
            c.LiftCellsImpl<T, Cell<T>, IReadOnlyList<T>>(v => v);

        /// <summary>
        ///     Lift into a collection of cells, so the returned cell always reflects a list of the input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <param name="c">The collection of cells.</param>
        /// <returns>A cell containing a list of the input cells' values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<IReadOnlyList<T>> Lift<T>(this IReadOnlyCollection<Cell<T>> c) =>
            c.LiftCellsImpl<T, Cell<T>, IReadOnlyList<T>>(v => v);
    }
}