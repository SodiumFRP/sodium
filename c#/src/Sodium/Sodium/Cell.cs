using System;

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
        public static Cell<T> Constant<T>(T value) => new Cell<T>(value);

        /// <summary>
        ///     Creates a cell with a lazily computed constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the cell.</typeparam>
        /// <param name="value">The lazily computed value of the cell.</param>
        /// <returns>A cell with a lazily computed constant value.</returns>
        public static Cell<T> ConstantLazy<T>(Lazy<T> value) => Stream.Never<T>().HoldLazyInternal(value);

        /// <summary>
        ///     Creates a writable cell that uses the last value if <see cref="CellSink{T}.Send" /> is called more than once per
        ///     transaction.
        /// </summary>
        /// <param name="initialValue">The initial value of the cell.</param>
        /// <typeparam name="T">The type of values in the cell sink.</typeparam>
        public static CellSink<T> CreateSink<T>(T initialValue) => new CellSink<T>(initialValue);

        /// <summary>
        ///     Creates a writable cell that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="CellSink{T}.Send" /> is called more than once per transaction.
        /// </summary>
        /// <param name="initialValue">The initial value of the cell.</param>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="CellSink{T}.Send(T)" /> is called more than once per
        ///     transaction.
        /// </param>
        /// <typeparam name="T">The type of values in the cell sink.</typeparam>
        public static CellSink<T> CreateSink<T>(T initialValue, Func<T, T, T> coalesce) =>
            new CellSink<T>(initialValue, coalesce);

        /// <summary>
        ///     Creates a <see cref="CellLoop{T}" />.  This must be called and looped from within the same transaction.
        /// </summary>
        /// <typeparam name="T">The type of values in the cell loop.</typeparam>
        public static CellLoop<T> CreateLoop<T>() => new CellLoop<T>();
    }

    /// <summary>
    ///     Represents a value that changes over time.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell.</typeparam>
    public class Cell<T>
    {
        private readonly Stream<T> stream;
        private Maybe<T> valueUpdate;

        // ReSharper disable once NotAccessedField.Local - Used to keep object from being garbage collected
        private readonly IListener streamListener;

        private T valueProperty;

        /// <summary>
        ///     Creates a cell with a constant value.
        /// </summary>
        /// <param name="value">The constant value of the cell.</param>
        internal Cell(T value)
        {
            this.stream = new Stream<T>();
            this.ValueProperty = value;
        }

        internal Cell(Stream<T> stream, T initialValue)
        {
            this.stream = stream;
            this.valueProperty = initialValue;
            this.UsingInitialValue = true;

            this.streamListener = Transaction.Apply(
                trans1 =>
                    this.stream.Listen(
                        Node<T>.Null,
                        trans1,
                        (trans2, a) =>
                        {
                            this.valueUpdate.MatchNone(
                                () =>
                                {
                                    trans2.Last(
                                        () =>
                                        {
                                            this.valueUpdate.MatchSome(v => this.ValueProperty = v);
                                            this.valueUpdate = Maybe.None;
                                        });
                                });

                            this.valueUpdate = Maybe.Some(a);
                        },
                        false),
                false);
        }

        internal IKeepListenersAlive KeepListenersAlive => this.stream.KeepListenersAlive;

        protected T ValueProperty
        {
            get => this.valueProperty;
            set
            {
                this.valueProperty = value;
                this.NotUsingInitialValue();
            }
        }

        protected virtual void NotUsingInitialValue()
        {
            this.UsingInitialValue = false;
        }

        protected bool UsingInitialValue { get; private set; }

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
        ///         It should generally be avoided in favor of <see cref="DiscreteCell{T}.Listen(Action{T})" />
        ///         for discrete cells so updates aren't missed, but in many circumstances it makes sense.
        ///     </para>
        ///     <para>
        ///         It can be best to use this method inside an explicit transaction (using
        ///         <see cref="Transaction.Run{T}(Func{T})" /> or <see cref="Transaction.RunVoid(Action)" />).
        ///         For example, a b.Sample() inside an explicit transaction along with a b.Updates().Listen(...) will capture the
        ///         current value and any updates without risk of missing any in between.
        ///     </para>
        /// </remarks>
        public T Sample() => Transaction.Apply(
            trans =>
            {
                if (trans.IsConstructing && !trans.ReachedClose)
                {
                    throw new InvalidOperationException(
                        "A cell may not be sampled during the construction phase of Transaction.RunConstruct.");
                }

                return this.SampleNoTransaction();
            },
            false);

        /// <summary>
        ///     Sample the current value of the cell lazily.
        /// </summary>
        /// <returns>A lazy which may be used to get the current value of the cell.</returns>
        /// <remarks>
        ///     This is a variant of <see cref="Sample" /> that works with the <see cref="CellLoop{T}" /> class
        ///     when the cell loop has not yet been looped.  It should be used in any code that is general
        ///     enough that it may be passed a <see cref="CellLoop{T}" />.  See <see cref="Stream{T}.HoldLazy(Lazy{T})" />.
        /// </remarks>
        public Lazy<T> SampleLazy() => Transaction.Apply(this.SampleLazy, false);

        internal Lazy<T> SampleLazy(Transaction trans)
        {
            LazySample s = new LazySample(this);
            trans.Last(
                () =>
                {
                    s.Value = this.valueUpdate.Match(v => v, this.SampleNoTransaction);
                    s.HasValue = true;
                    s.Cell = null;
                });
            return new Lazy<T>(() => s.HasValue ? s.Value : s.Cell.Sample());
        }

        internal virtual T SampleNoTransaction() => this.ValueProperty;

        internal Stream<T> Updates(Transaction trans) => this.stream;

        internal Stream<T> Value(Transaction trans1)
        {
            Stream<Unit> spark = new Stream<Unit>(this.stream.KeepListenersAlive);
            trans1.Prioritized(spark.Node, trans2 => spark.Send(trans2, Unit.Value));
            Stream<T> initial = spark.Snapshot(this);
            return initial.Merge(trans1, this.Updates(trans1), (left, right) => right);
        }

        /// <summary>
        ///     Transform the cell values according to the supplied function, so the returned
        ///     cell's values reflect the value of the function applied to the input cell's values.
        /// </summary>
        /// <typeparam name="TResult">The type of values fired by the returned cell.</typeparam>
        /// <param name="f">
        ///     Function to apply to convert the values.  It must be a pure function.
        /// </param>
        /// <returns>An cell which fires values transformed by <paramref name="f" /> for each value fired by this cell.</returns>
        public Cell<TResult> Map<TResult>(Func<T, TResult> f)
        {
            return Transaction.Apply(
                trans => this.Updates(trans).Map(f).HoldLazyInternal(this.SampleLazy(trans).Map(f)),
                false);
        }

        //      /**
        //* Lift a binary function into cells, so the returned Cell always reflects the specified
        //* function applied to the input cells' values.
        //* @param f Function to apply. It must be <em>referentially transparent</em>.
        //*/

        /// <summary>
        ///     Lift a binary function into cells, so the returned cell always reflects the specified function applied to the input
        ///     cells' values.
        /// </summary>
        /// <typeparam name="T2">The type of second cell.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <param name="b2">The second cell.</param>
        /// <returns>A cell containing values resulting from the binary function applied to the input cells' values.</returns>
        public Cell<TResult> Lift<T2, TResult>(Cell<T2> b2, Func<T, T2, TResult> f)
        {
            Func<T2, TResult> Ffa(T a) => b => f(a, b);
            return b2.Apply(this.Map(Ffa));
        }

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
        public Cell<TResult> Lift<T2, T3, TResult>(Cell<T2> b2, Cell<T3> b3, Func<T, T2, T3, TResult> f)
        {
            Func<T2, Func<T3, TResult>> Ffa(T a) => b => c => f(a, b, c);
            return b3.Apply(b2.Apply(this.Map(Ffa)));
        }

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
            Func<T, T2, T3, T4, TResult> f)
        {
            Func<T2, Func<T3, Func<T4, TResult>>> Ffa(T a) => b => c => d => f(a, b, c, d);
            return b4.Apply(b3.Apply(b2.Apply(this.Map(Ffa))));
        }

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
            Func<T, T2, T3, T4, T5, TResult> f)
        {
            Func<T2, Func<T3, Func<T4, Func<T5, TResult>>>> Ffa(T a) => b => c => d => e => f(a, b, c, d, e);
            return b5.Apply(b4.Apply(b3.Apply(b2.Apply(this.Map(Ffa)))));
        }

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
            Func<T, T2, T3, T4, T5, T6, TResult> f)
        {
            Func<T2, Func<T3, Func<T4, Func<T5, Func<T6, TResult>>>>> Ffa(T a) =>
                b => c => d => e => ff => f(a, b, c, d, e, ff);

            return b6.Apply(b5.Apply(b4.Apply(b3.Apply(b2.Apply(this.Map(Ffa))))));
        }

        /// <summary>
        ///     Apply a value inside a cell to a function inside a cell.  This is the primitive for all function lifting.
        /// </summary>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="bf">The cell containing the function to apply the value to.</param>
        /// <returns>
        ///     A cell whose value is the result of applying the current function in cell <paramref name="bf" /> to this
        ///     cell's current value.
        /// </returns>
        public Cell<TResult> Apply<TResult>(Cell<Func<T, TResult>> bf)
        {
            return Transaction.Apply(
                trans0 =>
                {
                    Stream<TResult> @out = new Stream<TResult>(this.stream.KeepListenersAlive);

                    Node<TResult> outTarget = @out.Node;
                    Node<Unit> inTarget = new Node<Unit>();
                    (bool changed, Node<Unit>.Target nodeTarget) = inTarget.Link(trans0, (t, v) => { }, outTarget);
                    if (changed)
                    {
                        trans0.SetNeedsRegenerating();
                    }

                    Func<T, TResult> f = null;
                    T a = default(T);
                    bool isASet = false;

                    // ReSharper disable once PossibleNullReferenceException
                    void H(Transaction trans1) => trans1.Prioritized(@out.Node, trans2 => @out.Send(trans2, f(a)));

                    IListener l1 = bf.Value(trans0)
                        .Listen(
                            inTarget,
                            trans0,
                            (trans1, ff) =>
                            {
                                f = ff;
                                if (isASet)
                                {
                                    H(trans1);
                                }
                            },
                            false);
                    IListener l2 = this.Value(trans0)
                        .Listen(
                            inTarget,
                            trans0,
                            (trans1, aa) =>
                            {
                                a = aa;
                                isASet = true;
                                if (f != null)
                                {
                                    H(trans1);
                                }
                            },
                            false);
                    return @out.LastFiringOnly(trans0)
                        .UnsafeAttachListener(l1)
                        .UnsafeAttachListener(l2)
                        .UnsafeAttachListener(
                            Listener.Create(inTarget, nodeTarget))
                        .HoldLazyInternal(
                            new Lazy<TResult>(() => bf.SampleNoTransaction()(this.SampleNoTransaction())));
                },
                false);
        }

        private class LazySample
        {
            internal Cell<T> Cell;
            internal bool HasValue;
            internal T Value;

            internal LazySample(Cell<T> cell) => this.Cell = cell;
        }
    }
}