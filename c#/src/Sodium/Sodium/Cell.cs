using System;
using System.Collections.Generic;

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
        public static Cell<T> Constant<T>(T value)
        {
            return new Cell<T>(value);
        }

        /// <summary>
        ///     Creates a cell with a lazily computed constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the cell.</typeparam>
        /// <param name="value">The lazily computed value of the cell.</param>
        /// <returns>A cell with a lazily computed constant value.</returns>
        public static Cell<T> ConstantLazy<T>(Lazy<T> value)
        {
            return Stream.Never<T>().HoldLazy(value);
        }
    }

    /// <summary>
    ///     Represents a value that changes over time.
    /// </summary>
    /// <typeparam name="T">The type of the value.</typeparam>
    public class Cell<T>
    {
        private readonly Stream<T> stream;
        private readonly MutableMaybeValue<T> valueUpdate = new MutableMaybeValue<T>();
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

            this.streamListener = Transaction.Apply(trans1 =>
                this.stream.Listen(Node<T>.Null, trans1, (trans2, a) =>
                {
                    this.valueUpdate.Get().Match(
                        v => { },
                        () =>
                        {
                            trans2.Last(() =>
                            {
                                this.valueUpdate.Get().Match(v => this.ValueProperty = v, () => { });
                                this.valueUpdate.Reset();
                            });
                        });

                    this.valueUpdate.Set(a);
                }, false));
        }

        protected T ValueProperty
        {
            get { return this.valueProperty; }
            set
            {
                this.valueProperty = value;
                this.UsingInitialValue = false;
            }
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
        ///         It should generally be avoided in favor of <see cref="Listen(Action{T})" />
        ///         so updates aren't missed, but in many circumstances it makes sense.
        ///     </para>
        ///     <para>
        ///         It can be best to use this method inside an explicit transaction (using
        ///         <see cref="Transaction.Run{T}(Func{T})" /> or <see cref="Transaction.RunVoid(Action)" />).
        ///         For example, a b.Sample() inside an explicit transaction along with a b.Updates().Listen(...) will capture the
        ///         current value and any updates without risk of missing any in between.
        ///     </para>
        /// </remarks>
        public T Sample() => Transaction.Apply(trans => this.SampleNoTransaction());

        /// <summary>
        ///     Sample the current value of the cell lazily.
        /// </summary>
        /// <returns>A lazy which may be used to get the current value of the cell.</returns>
        /// <remarks>
        ///     This is a variant of <see cref="Sample" /> that works with the <see cref="CellLoop{T}" /> class
        ///     when the cell loop has not yet been looped.  It should be used in any code that is general
        ///     enough that it may be passed a <see cref="CellLoop{T}" />.  See <see cref="Stream{T}.HoldLazy(Lazy{T})" />.
        /// </remarks>
        public Lazy<T> SampleLazy() => Transaction.Apply(this.SampleLazy);

        internal Lazy<T> SampleLazy(Transaction trans)
        {
            LazySample s = new LazySample(this);
            trans.Last(() =>
            {
                s.Value = this.valueUpdate.Get().Match(v => v, this.SampleNoTransaction);
                s.HasValue = true;
                s.Cell = null;
            });
            return new Lazy<T>(() => s.HasValue ? s.Value : s.Cell.Sample());
        }

        internal virtual T SampleNoTransaction()
        {
            return this.ValueProperty;
        }

        internal Stream<T> Updates(Transaction trans) => this.stream;

        internal Stream<T> Value(Transaction trans1)
        {
            Stream<Unit> spark = new Stream<Unit>();
            trans1.Prioritized(spark.Node, trans2 => spark.Send(trans2, Unit.Value));
            Stream<T> initial = spark.Snapshot(this);
            return initial.Merge(this.Updates(trans1), (left, right) => right);
        }

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
        public IListener Listen(Action<T> handler) => Transaction.Apply(trans => this.Value(trans).Listen(handler));

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
            return Transaction.Apply(trans => this.Updates(trans).Map(f).HoldLazy(trans, this.SampleLazy(trans).Map(f)));
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
            Func<T, Func<T2, TResult>> ffa = a => b => f(a, b);
            return b2.Apply(this.Map(ffa));
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
            Func<T, Func<T2, Func<T3, TResult>>> ffa = a => b => c => f(a, b, c);
            return b3.Apply(b2.Apply(this.Map(ffa)));
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
        public Cell<TResult> Lift<T2, T3, T4, TResult>(Cell<T2> b2, Cell<T3> b3, Cell<T4> b4, Func<T, T2, T3, T4, TResult> f)
        {
            Func<T, Func<T2, Func<T3, Func<T4, TResult>>>> ffa = a => b => c => d => f(a, b, c, d);
            return b4.Apply(b3.Apply(b2.Apply(this.Map(ffa))));
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
        public Cell<TResult> Lift<T2, T3, T4, T5, TResult>(Cell<T2> b2, Cell<T3> b3, Cell<T4> b4, Cell<T5> b5, Func<T, T2, T3, T4, T5, TResult> f)
        {
            Func<T, Func<T2, Func<T3, Func<T4, Func<T5, TResult>>>>> ffa = a => b => c => d => e => f(a, b, c, d, e);
            return b5.Apply(b4.Apply(b3.Apply(b2.Apply(this.Map(ffa)))));
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
        public Cell<TResult> Lift<T2, T3, T4, T5, T6, TResult>(Cell<T2> b2, Cell<T3> b3, Cell<T4> b4, Cell<T5> b5, Cell<T6> b6, Func<T, T2, T3, T4, T5, T6, TResult> f)
        {
            Func<T, Func<T2, Func<T3, Func<T4, Func<T5, Func<T6, TResult>>>>>> ffa = a => b => c => d => e => ff => f(a, b, c, d, e, ff);
            return b6.Apply(b5.Apply(b4.Apply(b3.Apply(b2.Apply(this.Map(ffa))))));
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
            return Transaction.Apply(trans0 =>
            {
                Stream<TResult> @out = new Stream<TResult>();

                Node<TResult> outTarget = @out.Node;
                Node<Unit> inTarget = new Node<Unit>(0);
                Node<Unit>.Target nodeTarget = inTarget.Link((t, v) => { }, outTarget).Item2;

                Func<T, TResult> f = null;
                T a = default(T);
                bool isASet = false;
                // ReSharper disable once PossibleNullReferenceException
                Action<Transaction> h = trans1 => trans1.Prioritized(@out.Node, trans2 => @out.Send(trans2, f(a)));
                IListener l1 = bf.Value(trans0).Listen(inTarget, (trans1, ff) =>
                {
                    f = ff;
                    if (isASet)
                    {
                        h(trans1);
                    }
                });
                IListener l2 = this.Value(trans0).Listen(inTarget, (trans1, aa) =>
                {
                    a = aa;
                    isASet = true;
                    if (f != null)
                    {
                        h(trans1);
                    }
                });
                return @out.LastFiringOnly(trans0).UnsafeAttachListener(l1).UnsafeAttachListener(l2).UnsafeAttachListener(
                    new Listener(() => inTarget.Unlink(nodeTarget))).HoldLazy(new Lazy<TResult>(() => bf.SampleNoTransaction()(this.SampleNoTransaction())));
            });
        }

        /// <summary>
        ///     Return a cell whose stream only receives events which have a different value than the previous event.
        /// </summary>
        /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
        public Cell<T> Calm()
        {
            return this.Calm(EqualityComparer<T>.Default);
        }

        /// <summary>
        ///     Return a cell whose stream only receives events which have a different value than the previous event.
        /// </summary>
        /// <param name="comparer">The equality comparer to use to determine if two items are equal.</param>
        /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
        public Cell<T> Calm(IEqualityComparer<T> comparer)
        {
            Lazy<T> initA = this.SampleLazy();
            Lazy<IMaybe<T>> mInitA = initA.Map(Maybe.Just);
            return Transaction.Apply(trans => this.Updates(trans).Calm(mInitA, comparer).HoldLazy(initA));
        }

        protected void NoOp()
        {
            GC.KeepAlive(this.streamListener);
        }

        private class LazySample
        {
            internal Cell<T> Cell;
            internal bool HasValue;
            internal T Value;

            internal LazySample(Cell<T> cell)
            {
                this.Cell = cell;
            }
        }
    }
}