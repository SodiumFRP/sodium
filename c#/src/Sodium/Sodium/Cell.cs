using System;
using System.Collections.Generic;
using JetBrains.Annotations;

namespace Sodium
{
    /// <summary>
    ///     Helper methods for creating a <see cref="Cell{T}" />.
    /// </summary>
    public static class Cell
    {
        /// <summary>
        ///     Creates a cell with a constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the cell.</typeparam>
        /// <param name="value">The value of the cell.</param>
        /// <returns>A cell with a constant value.</returns>
        public static Cell<T> Constant<T>(T value) =>
            new Cell<T>(Stream.Never<T>().HoldInternal(value));

        /// <summary>
        ///     Creates a cell with a lazy constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the cell.</typeparam>
        /// <param name="value">The lazy value of the cell.</param>
        /// <returns>A cell with a lazy constant value.</returns>
        public static Cell<T> ConstantLazy<T>(Lazy<T> value) =>
            new Cell<T>(Stream.Never<T>().HoldLazyInternal(value));

        /// <summary>
        ///     Construct a writable cell that uses the last value if <see cref="CellSink{T}.Send" /> is called
        ///     more than once per transaction.
        /// </summary>
        /// <param name="initialValue">The initial value of the cell.</param>
        public static CellSink<T> CreateSink<T>(T initialValue) => new CellSink<T>(initialValue);

        /// <summary>
        ///     Construct a writable cell stream sink that uses the last value if <see cref="CellSink{T}.Send" />
        ///     is called more than once per transaction.
        ///     This stream sink is meant to be turned into a <see cref="Cell{T}" /> through the use of
        ///     <see cref="CellStreamSink{T}.Hold(T)" />.
        /// </summary>
        /// <typeparam name="T">The type of values in the cell stream sink.</typeparam>
        public static CellStreamSink<T> CreateStreamSink<T>() => new CellStreamSink<T>();

        /// <summary>
        ///     Construct a writable cell stream sink that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="CellStreamSink{T}.Send(T)" /> is called more than once per transaction.
        ///     This stream sink is meant to be turned into a <see cref="Cell{T}" /> through the use of
        ///     <see cref="CellStreamSink{T}.Hold(T)" />.
        /// </summary>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="CellStreamSink{T}.Send(T)" /> is called more
        ///     than once per transaction.
        /// </param>
        /// <typeparam name="T">The type of values in the cell stream sink.</typeparam>
        public static CellStreamSink<T> CreateStreamSink<T>(Func<T, T, T> coalesce) =>
            new CellStreamSink<T>(coalesce);

        /// <summary>
        ///     Creates a cell loop.
        /// </summary>
        /// <typeparam name="T">The type of values in the cell loop.</typeparam>
        /// <returns>The cell loop.</returns>
        public static CellLoop<T> CreateLoop<T>() => new CellLoop<T>();

        /// <summary>
        ///     Creates a helper to loop over a cell for the specified type.
        /// </summary>
        /// <typeparam name="T">The type of the cell to loop.</typeparam>
        /// <returns>A <see cref="CellLooper{T}"/> which should be used to complete the loop.</returns>
        [Pure]
        public static CellLooper<T> Loop<T>() => new CellLooper<T>();
    }

    /// <summary>
    ///     A helper to complete a loop over a cell.
    /// </summary>
    /// <typeparam name="T">The type of the cell being looped.</typeparam>
    public struct CellLooper<T>
    {
        /// <summary>
        ///     Loop a cell and return a value tuple containing the resulting cell and captures.
        /// </summary>
        /// <typeparam name="TCaptures">The type of the captures to return.</typeparam>
        /// <param name="f">A function which takes the cell loop and returns a value tuple containing the resulting cell and captures.</param>
        /// <returns>A value tuple containing the resulting cell and captures.</returns>
        [Pure]
        public (Cell<T> Cell, TCaptures Captures) WithCaptures<TCaptures>(
            Func<LoopedCell<T>, (Cell<T> Cell, TCaptures Captures)> f) =>
            Transaction.Run(
                () =>
                {
                    LoopedCell<T> loop = new LoopedCell<T>();
                    (Cell<T> Cell, TCaptures Captures) result = f(loop);
                    loop.Loop(result.Cell);
                    return result;
                });

        /// <summary>
        ///     Loop a cell and return the resulting cell.
        /// </summary>
        /// <param name="f">A function which takes the cell loop and returns the resulting cell.</param>
        /// <returns>The resulting cell.</returns>
        [Pure]
        public Cell<T> WithoutCaptures(Func<LoopedCell<T>, Cell<T>> f) =>
            this.WithCaptures(l => (Cell: f(l), Captures: Unit.Value)).Cell;
    }

    /// <summary>
    ///     Represents a value that discretely changes over time.
    /// </summary>
    /// <typeparam name="T">The type of the value.</typeparam>
    public class Cell<T>
    {
        private readonly object updatesLock = new object();
        private Stream<T> updates;

        internal Cell(Behavior<T> behavior) => this.Behavior = behavior;

        /// <summary>
        ///     Sample the current value of the cell.
        /// </summary>
        /// <returns>The current value of the cell.</returns>
        /// <remarks>
        ///     <para>
        ///         This method may be used inside the functions passed to primitives that apply them to streams,
        ///         including <see cref="Stream{T}.Map{TResult}(Func{T, TResult})" /> in which case it is equivalent to
        ///         snapshotting the cell,
        ///         <see cref=" Stream{T}.Snapshot{T2, TResult}(Cell{T2}, Func{T, T2, TResult})" />,
        ///         <see cref="Stream{T}.Filter(Func{T, bool})" />, and
        ///         <see cref="Stream{T}.Merge(Stream{T}, Func{T, T, T})" />
        ///     </para>
        ///     <para>
        ///         It should generally be avoided in favor of <see cref="Listen(Action{T})" />
        ///         so updates aren't missed, but in many circumstances it makes sense.
        ///     </para>
        ///     <para>
        ///         It can be best to use this method inside an explicit transaction (using
        ///         <see cref="Transaction.Run{T}(Func{T})" /> or <see cref="Transaction.RunVoid(Action)" />).
        ///         For example, a c.Sample() inside an explicit transaction along with a c.Updates().Listen(...) will capture the
        ///         current value and any updates without risk of missing any in between.
        ///     </para>
        /// </remarks>
        public T Sample() => this.Behavior.Sample();

        /// <summary>
        ///     Sample the current value of the cell lazily.
        /// </summary>
        /// <returns>A lazy which may be used to get the current value of the cell.</returns>
        /// <remarks>
        ///     This is a variant of <see cref="Sample" /> that works with the <see cref="CellLoop{T}" /> class
        ///     when the cell loop has not yet been looped.  It should be used in any code that is general
        ///     enough that it may be passed a <see cref="CellLoop{T}" />.  See <see cref="Stream{T}.HoldLazy(Lazy{T})" />.
        /// </remarks>
        public Lazy<T> SampleLazy() => this.Behavior.SampleLazy();

        /// <summary>
        ///     The stream of discrete updates to this cell.
        /// </summary>
        public virtual Stream<T> Updates
        {
            get
            {
                lock (this.updatesLock)
                {
                    return this.updates ?? (this.updates = Transaction.Apply(
                               trans => this.Behavior.Updates(trans).Coalesce(trans, (left, right) => right),
                               false));
                }
            }
        }

        /// <summary>
        ///     The stream of values of this cell.  This stream is identical to <see cref="Updates" /> except that it also fires
        ///     during the transaction in which it was obtained.
        ///     To observe the first value, this property must be accessed and used within the same explicit transaction.
        /// </summary>
        public virtual Stream<T> Values
        {
            get { return Transaction.Apply(trans => this.Behavior.Value(trans), false); }
        }

        /// <summary>
        ///     The underlying behavior holding the current value of this cell.
        /// </summary>
        internal Behavior<T> Behavior { get; }

        /// <summary>
        ///     Return a reference to this <see cref="Cell{T}" /> as a <see cref="Behavior{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="Cell{T}" /> as a <see cref="Behavior{T}" />.</returns>
        public Behavior<T> AsBehavior() => this.Behavior;

        /// <summary>
        ///     Listen for updates to the value of this cell.  The returned <see cref="IListener" /> may be
        ///     disposed to stop listening.  This is an OPERATIONAL mechanism for interfacing between
        ///     the world of I/O and FRP.
        /// </summary>
        /// <param name="handler">The handler to execute for each value.</param>
        /// <returns>An <see cref="IListener" /> which may be disposed to stop listening.</returns>
        /// <remarks>
        ///     <para>
        ///         No assumptions should be made about what thread the handler is called on and it should not block.
        ///         Neither <see cref="StreamSink{T}.Send" /> nor <see cref="CellSink{T}.Send" /> may be called from the
        ///         handler.
        ///         They will throw an exception because this method is not meant to be used to create new primitives.
        ///     </para>
        ///     <para>
        ///         If the <see cref="IListener" /> is not disposed, it will continue to listen until this cell is either
        ///         disposed or garbage collected.
        ///     </para>
        /// </remarks>
        public IStrongListener Listen(Action<T> handler) => Transaction.Apply(
            trans => this.Behavior.Value(trans).Listen(handler),
            false);

        /// <summary>
        ///     Listen for updates to the value of this cell.  The returned <see cref="IListener" /> may be
        ///     disposed to stop listening, or it will automatically stop listening when it is garbage collected.
        ///     This is an OPERATIONAL mechanism for interfacing between the world of I/O and FRP.
        /// </summary>
        /// <param name="handler">The handler to execute for each value.</param>
        /// <returns>An <see cref="IListener" /> which may be disposed to stop listening.</returns>
        /// <remarks>
        ///     <para>
        ///         No assumptions should be made about what thread the handler is called on and it should not block.
        ///         Neither <see cref="StreamSink{T}.Send" /> nor <see cref="CellSink{T}.Send" /> may be called from the
        ///         handler.
        ///         They will throw an exception because this method is not meant to be used to create new primitives.
        ///     </para>
        ///     <para>
        ///         If the <see cref="IListener" /> is not disposed, it will continue to listen until this cell is either
        ///         disposed or garbage collected or the listener itself is garbage collected.
        ///     </para>
        /// </remarks>
        public IWeakListener ListenWeak(Action<T> handler) => Transaction.Apply(
            trans => this.Behavior.Value(trans).ListenWeak(handler),
            false);

        /// <summary>
        ///     Transform the cell values according to the supplied function, so the returned
        ///     cell's values reflect the value of the function applied to the input cell's values.
        /// </summary>
        /// <typeparam name="TResult">The type of values fired by the returned cell.</typeparam>
        /// <param name="f">
        ///     Function to apply to convert the values.  It must be a pure function.
        /// </param>
        /// <returns>A cell which fires values transformed by <paramref name="f" /> for each value fired by this cell.</returns>
        public Cell<TResult> Map<TResult>(Func<T, TResult> f) =>
            new Cell<TResult>(this.Behavior.Map(f));

        /// <summary>
        ///     Lift a binary function into cells, so the returned cell always reflects the specified function applied to the input
        ///     cells' values.
        /// </summary>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <param name="b2">The second cell.</param>
        /// <returns>A cell containing values resulting from the binary function applied to the input cells' values.</returns>
        public Cell<TResult> Lift<T2, TResult>(Cell<T2> b2, Func<T, T2, TResult> f) =>
            new Cell<TResult>(this.Behavior.Lift(b2.Behavior, f));

        /// <summary>
        ///     Lift a ternary function into cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="T3">The type of third cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <param name="b2">The second cell.</param>
        /// <param name="b3">The third cell.</param>
        /// <returns>A cell containing values resulting from the ternary function applied to the input cells' values.</returns>
        public Cell<TResult> Lift<T2, T3, TResult>(
            Cell<T2> b2,
            Cell<T3> b3,
            Func<T, T2, T3, TResult> f) =>
            new Cell<TResult>(this.Behavior.Lift(b2.Behavior, b3.Behavior, f));

        /// <summary>
        ///     Lift a quaternary function into cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="T3">The type of third cell.</typeparam>
        /// <typeparam name="T4">The type of fourth cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <param name="b2">The second cell.</param>
        /// <param name="b3">The third cell.</param>
        /// <param name="b4">The fourth cell.</param>
        /// <returns>A cell containing values resulting from the quaternary function applied to the input cells' values.</returns>
        public Cell<TResult> Lift<T2, T3, T4, TResult>(
            Cell<T2> b2,
            Cell<T3> b3,
            Cell<T4> b4,
            Func<T, T2, T3, T4, TResult> f) =>
            new Cell<TResult>(this.Behavior.Lift(b2.Behavior, b3.Behavior, b4.Behavior, f));

        /// <summary>
        ///     Lift a 5-argument function into cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="T3">The type of third cell.</typeparam>
        /// <typeparam name="T4">The type of fourth cell.</typeparam>
        /// <typeparam name="T5">The type of fifth cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <param name="b2">The second cell.</param>
        /// <param name="b3">The third cell.</param>
        /// <param name="b4">The fourth cell.</param>
        /// <param name="b5">The fifth cell.</param>
        /// <returns>A cell containing values resulting from the 5-argument function applied to the input cells' values.</returns>
        public Cell<TResult> Lift<T2, T3, T4, T5, TResult>(
            Cell<T2> b2,
            Cell<T3> b3,
            Cell<T4> b4,
            Cell<T5> b5,
            Func<T, T2, T3, T4, T5, TResult> f) =>
            new Cell<TResult>(this.Behavior.Lift(b2.Behavior, b3.Behavior, b4.Behavior, b5.Behavior, f));

        /// <summary>
        ///     Lift a 6-argument function into cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="T3">The type of third cell.</typeparam>
        /// <typeparam name="T4">The type of fourth cell.</typeparam>
        /// <typeparam name="T5">The type of fifth cell.</typeparam>
        /// <typeparam name="T6">The type of sixth cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <param name="b2">The second cell.</param>
        /// <param name="b3">The third cell.</param>
        /// <param name="b4">The fourth cell.</param>
        /// <param name="b5">The fifth cell.</param>
        /// <param name="b6">The sixth cell.</param>
        /// <returns>A cell containing values resulting from the 6-argument function applied to the input cells' values.</returns>
        public Cell<TResult> Lift<T2, T3, T4, T5, T6, TResult>(
            Cell<T2> b2,
            Cell<T3> b3,
            Cell<T4> b4,
            Cell<T5> b5,
            Cell<T6> b6,
            Func<T, T2, T3, T4, T5, T6, TResult> f) =>
            new Cell<TResult>(this.Behavior.Lift(b2.Behavior, b3.Behavior, b4.Behavior, b5.Behavior, b6.Behavior, f));

        /// <summary>
        ///     Apply a value inside a cell to a function inside a cell.  This is the primitive for all function lifting.
        /// </summary>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="bf">The cell containing the function to apply the value to.</param>
        /// <returns>
        ///     A cell whose value is the result of applying the current function in cell <paramref name="bf" /> to this
        ///     cell's current value.
        /// </returns>
        public Cell<TResult> Apply<TResult>(Cell<Func<T, TResult>> bf) =>
            new Cell<TResult>(this.Behavior.Apply(bf.Behavior));

        /// <summary>
        ///     Return a cell whose stream only receives events which have a different value than the previous event.
        /// </summary>
        /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
        public Cell<T> Calm() => this.Calm(EqualityComparer<T>.Default);

        /// <summary>
        ///     Return a cell whose stream only receives events which have a different value than the previous event.
        /// </summary>
        /// <param name="comparer">The equality comparer to use to determine if two items are equal.</param>
        /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
        public Cell<T> Calm(IEqualityComparer<T> comparer)
        {
            Lazy<T> initA = this.Behavior.SampleLazy();
            Lazy<Maybe<T>> mInitA = initA.Map(Maybe.Some);
            return Transaction.Apply(trans => this.Behavior.Updates(trans).Calm(mInitA, comparer).HoldLazy(initA), false);
        }
    }
}