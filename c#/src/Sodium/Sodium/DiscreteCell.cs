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
        ///     Creates a discrete cell with the specified initial value using the given stream for updates.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="stream">The stream which will provide updates to the discrete cell.</param>
        /// <param name="initialValue">The initial value of the discrete cell.</param>
        /// <returns>A discrete cell with initial value <see cref="initialValue"/> receiving updates from <see cref="stream"/>.</returns>
        public static DiscreteCell<T> Create<T>(Stream<T> stream, Lazy<T> initialValue) => new DiscreteCell<T>(stream, stream.HoldLazy(initialValue));

        /// <summary>
        ///     Creates a discrete cell with a constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the cell.</typeparam>
        /// <param name="value">The value of the cell.</param>
        /// <returns>A discrete cell with a constant value.</returns>
        public static DiscreteCell<T> Constant<T>(T value) => Create(Stream.Never<T>(), new Lazy<T>(() => value));

        /// <summary>
        ///     Creates a discrete cell loop.
        /// </summary>
        /// <typeparam name="T">The type of values in the discrete cell loop.</typeparam>
        /// <returns>The discrete cell loop.</returns>
        public static DiscreteCellLoop<T> CreateLoop<T>() => new DiscreteCellLoop<T>();

        /// <summary>
        ///     Construct a writable discrete cell that uses the last value if <see cref="DiscreteCellSink{T}.Send" /> is called more than once per transaction.
        /// </summary>
        /// <param name="initialValue">The initial value of the discrete cell.</param>
        public static DiscreteCellSink<T> CreateSink<T>(T initialValue) => new DiscreteCellSink<T>(initialValue);
    }

    /// <summary>
    ///     Represents a value that discretely changes over time.
    /// </summary>
    /// <typeparam name="T">The type of the value.</typeparam>
    public class DiscreteCell<T>
    {
        internal DiscreteCell(Stream<T> stream, Cell<T> cell)
        {
            this.Updates = stream;
            this.Cell = cell;
        }

        /// <summary>
        ///     The stream of discrete updates to this cell.
        /// </summary>
        public Stream<T> Updates { get; }

        /// <summary>
        ///     The underlying cell holding the current value of this discrete cell.
        /// </summary>
        public Cell<T> Cell { get; }

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
            Transaction.Run(() => DiscreteCell.Create(this.Updates.Map(f), this.Cell.SampleLazy().Map(f)));

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
            Transaction.Run(
                () => DiscreteCell.Create(
                    this.Updates.Map(ModifyTuple<T, T2>)
                        .Merge(b2.Updates.Map(ModifyTuple<T, T2>), Compose)
                        .Snapshot(this.Cell.Lift(b2.Cell, Tuple.Create), (ff, x) => ff(x))
                        .Map(o => f(o.Item1, o.Item2)),
                    new Lazy<TResult>(() => f(this.Cell.Sample(), b2.Cell.Sample()))));

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
        public DiscreteCell<TResult> Lift<T2, T3, TResult>(DiscreteCell<T2> b2, DiscreteCell<T3> b3, Func<T, T2, T3, TResult> f) =>
            Transaction.Run(
                () => DiscreteCell.Create(
                    this.Updates.Map(ModifyTuple<T, T2, T3>)
                        .Merge(b2.Updates.Map(ModifyTuple<T, T2, T3>), Compose)
                        .Merge(b3.Updates.Map(ModifyTuple<T, T2, T3>), Compose)
                        .Snapshot(this.Cell.Lift(b2.Cell, b3.Cell, Tuple.Create), (ff, x) => ff(x))
                        .Map(o => f(o.Item1, o.Item2, o.Item3)),
                    new Lazy<TResult>(() => f(this.Cell.Sample(), b2.Cell.Sample(), b3.Cell.Sample()))));

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
        public DiscreteCell<TResult> Lift<T2, T3, T4, TResult>(DiscreteCell<T2> b2, DiscreteCell<T3> b3, DiscreteCell<T4> b4, Func<T, T2, T3, T4, TResult> f) =>
            Transaction.Run(
                () => DiscreteCell.Create(
                    this.Updates.Map(ModifyTuple<T, T2, T3, T4>)
                        .Merge(b2.Updates.Map(ModifyTuple<T, T2, T3, T4>), Compose)
                        .Merge(b3.Updates.Map(ModifyTuple<T, T2, T3, T4>), Compose)
                        .Merge(b4.Updates.Map(ModifyTuple<T, T2, T3, T4>), Compose)
                        .Snapshot(this.Cell.Lift(b2.Cell, b3.Cell, b4.Cell, Tuple.Create), (ff, x) => ff(x))
                        .Map(o => f(o.Item1, o.Item2, o.Item3, o.Item4)),
                    new Lazy<TResult>(() => f(this.Cell.Sample(), b2.Cell.Sample(), b3.Cell.Sample(), b4.Cell.Sample()))));

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
        public DiscreteCell<TResult> Lift<T2, T3, T4, T5, TResult>(DiscreteCell<T2> b2, DiscreteCell<T3> b3, DiscreteCell<T4> b4, DiscreteCell<T5> b5, Func<T, T2, T3, T4, T5, TResult> f) =>
            Transaction.Run(
                () => DiscreteCell.Create(
                    this.Updates.Map(ModifyTuple<T, T2, T3, T4, T5>)
                        .Merge(b2.Updates.Map(ModifyTuple<T, T2, T3, T4, T5>), Compose)
                        .Merge(b3.Updates.Map(ModifyTuple<T, T2, T3, T4, T5>), Compose)
                        .Merge(b4.Updates.Map(ModifyTuple<T, T2, T3, T4, T5>), Compose)
                        .Merge(b5.Updates.Map(ModifyTuple<T, T2, T3, T4, T5>), Compose)
                        .Snapshot(this.Cell.Lift(b2.Cell, b3.Cell, b4.Cell, b5.Cell, Tuple.Create), (ff, x) => ff(x))
                        .Map(o => f(o.Item1, o.Item2, o.Item3, o.Item4, o.Item5)),
                    new Lazy<TResult>(() => f(this.Cell.Sample(), b2.Cell.Sample(), b3.Cell.Sample(), b4.Cell.Sample(), b5.Cell.Sample()))));

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
        public DiscreteCell<TResult> Lift<T2, T3, T4, T5, T6, TResult>(DiscreteCell<T2> b2, DiscreteCell<T3> b3, DiscreteCell<T4> b4, DiscreteCell<T5> b5, DiscreteCell<T6> b6, Func<T, T2, T3, T4, T5, T6, TResult> f) =>
            Transaction.Run(
                () => DiscreteCell.Create(
                    this.Updates.Map(ModifyTuple<T, T2, T3, T4, T5, T6>)
                        .Merge(b2.Updates.Map(ModifyTuple<T, T2, T3, T4, T5, T6>), Compose)
                        .Merge(b3.Updates.Map(ModifyTuple<T, T2, T3, T4, T5, T6>), Compose)
                        .Merge(b4.Updates.Map(ModifyTuple<T, T2, T3, T4, T5, T6>), Compose)
                        .Merge(b5.Updates.Map(ModifyTuple<T, T2, T3, T4, T5, T6>), Compose)
                        .Merge(b6.Updates.Map(ModifyTuple<T, T2, T3, T4, T5, T6>), Compose)
                        .Snapshot(this.Cell.Lift(b2.Cell, b3.Cell, b4.Cell, b5.Cell, b6.Cell, Tuple.Create), (ff, x) => ff(x))
                        .Map(o => f(o.Item1, o.Item2, o.Item3, o.Item4, o.Item5, o.Item6)),
                    new Lazy<TResult>(() => f(this.Cell.Sample(), b2.Cell.Sample(), b3.Cell.Sample(), b4.Cell.Sample(), b5.Cell.Sample(), b6.Cell.Sample()))));

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
            Transaction.Run(
                () => DiscreteCell.Create(
                    this.Updates.Map(ModifyTuple<T, Func<T, TResult>>)
                        .Merge(bf.Updates.Map(ModifyTuple<T, Func<T, TResult>>), Compose)
                        .Snapshot(this.Cell.Lift(bf.Cell, Tuple.Create), (ff, x) => ff(x))
                        .Map(o => o.Item2(o.Item1)),
                    bf.Cell.SampleLazy().Map(f => f(this.Cell.Sample()))));

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
        public DiscreteCell<T> Calm(IEqualityComparer<T> comparer) =>
            Transaction.Run(() =>
            {
                Lazy<T> initialValue = this.Cell.SampleLazy();
                return DiscreteCell.Create(
                    this.Updates.CollectLazy(initialValue.Map(Maybe.Just), (a, lastA) =>
                    {
                        if (lastA.Match(v => comparer.Equals(v, a), () => false))
                        {
                            return Tuple.Create(Maybe.Nothing<T>(), lastA);
                        }

                        IMaybe<T> ma = Maybe.Just(a);
                        return Tuple.Create(ma, ma);
                    }).FilterMaybe(),
                    initialValue);
            });

        private static Func<TFunction, TFunction> Compose<TFunction>(Func<TFunction, TFunction> f, Func<TFunction, TFunction> g) => x => g(f(x));

        private static Func<Tuple<T1, T2>, Tuple<T1, T2>> ModifyTuple<T1, T2>(T1 value) => o => Tuple.Create(value, o.Item2);
        private static Func<Tuple<T1, T2>, Tuple<T1, T2>> ModifyTuple<T1, T2>(T2 value) => o => Tuple.Create(o.Item1, value);

        private static Func<Tuple<T1, T2, T3>, Tuple<T1, T2, T3>> ModifyTuple<T1, T2, T3>(T1 value) => o => Tuple.Create(value, o.Item2, o.Item3);
        private static Func<Tuple<T1, T2, T3>, Tuple<T1, T2, T3>> ModifyTuple<T1, T2, T3>(T2 value) => o => Tuple.Create(o.Item1, value, o.Item3);
        private static Func<Tuple<T1, T2, T3>, Tuple<T1, T2, T3>> ModifyTuple<T1, T2, T3>(T3 value) => o => Tuple.Create(o.Item1, o.Item2, value);

        private static Func<Tuple<T1, T2, T3, T4>, Tuple<T1, T2, T3, T4>> ModifyTuple<T1, T2, T3, T4>(T1 value) => o => Tuple.Create(value, o.Item2, o.Item3, o.Item4);
        private static Func<Tuple<T1, T2, T3, T4>, Tuple<T1, T2, T3, T4>> ModifyTuple<T1, T2, T3, T4>(T2 value) => o => Tuple.Create(o.Item1, value, o.Item3, o.Item4);
        private static Func<Tuple<T1, T2, T3, T4>, Tuple<T1, T2, T3, T4>> ModifyTuple<T1, T2, T3, T4>(T3 value) => o => Tuple.Create(o.Item1, o.Item2, value, o.Item4);
        private static Func<Tuple<T1, T2, T3, T4>, Tuple<T1, T2, T3, T4>> ModifyTuple<T1, T2, T3, T4>(T4 value) => o => Tuple.Create(o.Item1, o.Item2, o.Item3, value);

        private static Func<Tuple<T1, T2, T3, T4, T5>, Tuple<T1, T2, T3, T4, T5>> ModifyTuple<T1, T2, T3, T4, T5>(T1 value) => o => Tuple.Create(value, o.Item2, o.Item3, o.Item4, o.Item5);
        private static Func<Tuple<T1, T2, T3, T4, T5>, Tuple<T1, T2, T3, T4, T5>> ModifyTuple<T1, T2, T3, T4, T5>(T2 value) => o => Tuple.Create(o.Item1, value, o.Item3, o.Item4, o.Item5);
        private static Func<Tuple<T1, T2, T3, T4, T5>, Tuple<T1, T2, T3, T4, T5>> ModifyTuple<T1, T2, T3, T4, T5>(T3 value) => o => Tuple.Create(o.Item1, o.Item2, value, o.Item4, o.Item5);
        private static Func<Tuple<T1, T2, T3, T4, T5>, Tuple<T1, T2, T3, T4, T5>> ModifyTuple<T1, T2, T3, T4, T5>(T4 value) => o => Tuple.Create(o.Item1, o.Item2, o.Item3, value, o.Item5);
        private static Func<Tuple<T1, T2, T3, T4, T5>, Tuple<T1, T2, T3, T4, T5>> ModifyTuple<T1, T2, T3, T4, T5>(T5 value) => o => Tuple.Create(o.Item1, o.Item2, o.Item3, o.Item4, value);

        private static Func<Tuple<T1, T2, T3, T4, T5, T6>, Tuple<T1, T2, T3, T4, T5, T6>> ModifyTuple<T1, T2, T3, T4, T5, T6>(T1 value) => o => Tuple.Create(value, o.Item2, o.Item3, o.Item4, o.Item5, o.Item6);
        private static Func<Tuple<T1, T2, T3, T4, T5, T6>, Tuple<T1, T2, T3, T4, T5, T6>> ModifyTuple<T1, T2, T3, T4, T5, T6>(T2 value) => o => Tuple.Create(o.Item1, value, o.Item3, o.Item4, o.Item5, o.Item6);
        private static Func<Tuple<T1, T2, T3, T4, T5, T6>, Tuple<T1, T2, T3, T4, T5, T6>> ModifyTuple<T1, T2, T3, T4, T5, T6>(T3 value) => o => Tuple.Create(o.Item1, o.Item2, value, o.Item4, o.Item5, o.Item6);
        private static Func<Tuple<T1, T2, T3, T4, T5, T6>, Tuple<T1, T2, T3, T4, T5, T6>> ModifyTuple<T1, T2, T3, T4, T5, T6>(T4 value) => o => Tuple.Create(o.Item1, o.Item2, o.Item3, value, o.Item5, o.Item6);
        private static Func<Tuple<T1, T2, T3, T4, T5, T6>, Tuple<T1, T2, T3, T4, T5, T6>> ModifyTuple<T1, T2, T3, T4, T5, T6>(T5 value) => o => Tuple.Create(o.Item1, o.Item2, o.Item3, o.Item4, value, o.Item6);
        private static Func<Tuple<T1, T2, T3, T4, T5, T6>, Tuple<T1, T2, T3, T4, T5, T6>> ModifyTuple<T1, T2, T3, T4, T5, T6>(T6 value) => o => Tuple.Create(o.Item1, o.Item2, o.Item3, o.Item4, o.Item5, value);
    }
}
