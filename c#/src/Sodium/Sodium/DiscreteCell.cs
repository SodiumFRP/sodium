using System;
using System.Collections.Generic;

namespace Sodium
{
    /// <summary>
    ///     Helper methods for creating a <see cref="DiscreteCell{T}" />.
    /// </summary>
    public static class DiscreteCell
    {
        /// <summary>
        ///     Creates a discrete cell with a constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the cell.</typeparam>
        /// <param name="value">The value of the cell.</param>
        /// <returns>A discrete cell with a constant value.</returns>
        public static DiscreteCell<T> Constant<T>(T value) =>
            new DiscreteCell<T>(Stream.Never<T>().HoldInternal(value));

        /// <summary>
        ///     Creates a discrete cell with a lazy constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the cell.</typeparam>
        /// <param name="value">The lazy value of the cell.</param>
        /// <returns>A discrete cell with a lazy constant value.</returns>
        public static DiscreteCell<T> ConstantLazy<T>(Lazy<T> value) =>
            new DiscreteCell<T>(Stream.Never<T>().HoldLazyInternal(value));

        /// <summary>
        ///     Creates a discrete cell loop.
        /// </summary>
        /// <typeparam name="T">The type of values in the discrete cell loop.</typeparam>
        /// <returns>The discrete cell loop.</returns>
        public static DiscreteCellLoop<T> CreateLoop<T>() => new DiscreteCellLoop<T>();

        /// <summary>
        ///     Construct a writable discrete cell that uses the last value if <see cref="DiscreteCellSink{T}.Send" /> is called
        ///     more than once per transaction.
        /// </summary>
        /// <param name="initialValue">The initial value of the discrete cell.</param>
        public static DiscreteCellSink<T> CreateSink<T>(T initialValue) => new DiscreteCellSink<T>(initialValue);

        /// <summary>
        ///     Construct a writable discrete cell stream sink that uses the last value if <see cref="DiscreteCellSink{T}.Send" />
        ///     is called more than once per transaction.
        ///     This stream sink is meant to be turned into a <see cref="DiscreteCell{T}" /> through the use of
        ///     <see cref="DiscreteCellStreamSink{T}.Hold(T)" />.
        /// </summary>
        /// <typeparam name="T">The type of values in the discrete cell stream sink.</typeparam>
        public static DiscreteCellStreamSink<T> CreateStreamSink<T>() => new DiscreteCellStreamSink<T>();

        /// <summary>
        ///     Construct a writable discrete cell stream sink that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="DiscreteCellStreamSink{T}.Send(T)" /> is called more than once per transaction.
        ///     This stream sink is meant to be turned into a <see cref="DiscreteCell{T}" /> through the use of
        ///     <see cref="DiscreteCellStreamSink{T}.Hold(T)" />.
        /// </summary>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="DiscreteCellStreamSink{T}.Send(T)" /> is called more
        ///     than once per transaction.
        /// </param>
        /// <typeparam name="T">The type of values in the discrete cell stream sink.</typeparam>
        public static DiscreteCellStreamSink<T> CreateStreamSink<T>(Func<T, T, T> coalesce) =>
            new DiscreteCellStreamSink<T>(coalesce);
    }

    /// <summary>
    ///     Represents a value that discretely changes over time.
    /// </summary>
    /// <typeparam name="T">The type of the value.</typeparam>
    public class DiscreteCell<T>
    {
        private readonly object updatesLock = new object();
        private Stream<T> updates;

        internal DiscreteCell(Cell<T> cell) => this.Cell = cell;

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
                               trans => this.Cell.Updates(trans).Coalesce(trans, (left, right) => right),
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
            get { return Transaction.Apply(trans => this.Cell.Value(trans), false); }
        }

        /// <summary>
        ///     The underlying cell holding the current value of this discrete cell.
        /// </summary>
        public Cell<T> Cell { get; }

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
            trans => this.Cell.Value(trans).Listen(handler),
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
            trans => this.Cell.Value(trans).ListenWeak(handler),
            false);

        /// <summary>
        ///     Transform the cell values according to the supplied function, so the returned
        ///     cell's values reflect the value of the function applied to the input cell's values.
        /// </summary>
        /// <typeparam name="TResult">The type of values fired by the returned cell.</typeparam>
        /// <param name="f">
        ///     Function to apply to convert the values.  It must be a pure function.
        /// </param>
        /// <returns>A discrete cell which fires values transformed by <paramref name="f" /> for each value fired by this cell.</returns>
        public DiscreteCell<TResult> Map<TResult>(Func<T, TResult> f) =>
            new DiscreteCell<TResult>(this.Cell.Map(f));

        /// <summary>
        ///     Lift a binary function into cells, so the returned cell always reflects the specified function applied to the input
        ///     cells' values.
        /// </summary>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <param name="b2">The second cell.</param>
        /// <returns>A discrete cell containing values resulting from the binary function applied to the input cells' values.</returns>
        public DiscreteCell<TResult> Lift<T2, TResult>(DiscreteCell<T2> b2, Func<T, T2, TResult> f) =>
            new DiscreteCell<TResult>(this.Cell.Lift(b2.Cell, f));

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
        /// <returns>A discrete cell containing values resulting from the ternary function applied to the input cells' values.</returns>
        public DiscreteCell<TResult> Lift<T2, T3, TResult>(
            DiscreteCell<T2> b2,
            DiscreteCell<T3> b3,
            Func<T, T2, T3, TResult> f) =>
            new DiscreteCell<TResult>(this.Cell.Lift(b2.Cell, b3.Cell, f));

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
        /// <returns>A discrete cell containing values resulting from the quaternary function applied to the input cells' values.</returns>
        public DiscreteCell<TResult> Lift<T2, T3, T4, TResult>(
            DiscreteCell<T2> b2,
            DiscreteCell<T3> b3,
            DiscreteCell<T4> b4,
            Func<T, T2, T3, T4, TResult> f) =>
            new DiscreteCell<TResult>(this.Cell.Lift(b2.Cell, b3.Cell, b4.Cell, f));

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
        /// <returns>A discrete cell containing values resulting from the 5-argument function applied to the input cells' values.</returns>
        public DiscreteCell<TResult> Lift<T2, T3, T4, T5, TResult>(
            DiscreteCell<T2> b2,
            DiscreteCell<T3> b3,
            DiscreteCell<T4> b4,
            DiscreteCell<T5> b5,
            Func<T, T2, T3, T4, T5, TResult> f) =>
            new DiscreteCell<TResult>(this.Cell.Lift(b2.Cell, b3.Cell, b4.Cell, b5.Cell, f));

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
        /// <returns>A discrete cell containing values resulting from the 6-argument function applied to the input cells' values.</returns>
        public DiscreteCell<TResult> Lift<T2, T3, T4, T5, T6, TResult>(
            DiscreteCell<T2> b2,
            DiscreteCell<T3> b3,
            DiscreteCell<T4> b4,
            DiscreteCell<T5> b5,
            DiscreteCell<T6> b6,
            Func<T, T2, T3, T4, T5, T6, TResult> f) =>
            new DiscreteCell<TResult>(this.Cell.Lift(b2.Cell, b3.Cell, b4.Cell, b5.Cell, b6.Cell, f));

        /// <summary>
        ///     Apply a value inside a cell to a function inside a cell.  This is the primitive for all function lifting.
        /// </summary>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="bf">The cell containing the function to apply the value to.</param>
        /// <returns>
        ///     A discrete cell whose value is the result of applying the current function in cell <paramref name="bf" /> to this
        ///     cell's current value.
        /// </returns>
        public DiscreteCell<TResult> Apply<TResult>(DiscreteCell<Func<T, TResult>> bf) =>
            new DiscreteCell<TResult>(this.Cell.Apply(bf.Cell));

        /// <summary>
        ///     Return a discrete cell whose stream only receives events which have a different value than the previous event.
        /// </summary>
        /// <returns>A discrete cell whose stream only receives events which have a different value than the previous event.</returns>
        public DiscreteCell<T> Calm() => this.Calm(EqualityComparer<T>.Default);

        /// <summary>
        ///     Return a discrete cell whose stream only receives events which have a different value than the previous event.
        /// </summary>
        /// <param name="comparer">The equality comparer to use to determine if two items are equal.</param>
        /// <returns>A discrete cell whose stream only receives events which have a different value than the previous event.</returns>
        public DiscreteCell<T> Calm(IEqualityComparer<T> comparer)
        {
            Lazy<T> initA = this.Cell.SampleLazy();
            Lazy<Maybe<T>> mInitA = initA.Map(Maybe.Some);
            return Transaction.Apply(trans => this.Cell.Updates(trans).Calm(mInitA, comparer).HoldLazy(initA), false);
        }
    }
}