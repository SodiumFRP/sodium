using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public static class CellExtensionMethods
    {
        /// <summary>
        ///     Unwrap a cell inside another cell to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="cca">The cell containing another cell.</param>
        /// <returns>The unwrapped cell.</returns>
        public static Cell<T> SwitchC<T>(this Cell<Cell<T>> cca)
        {
            return Transaction.Apply(trans1 =>
            {
                Lazy<T> za = cca.SampleLazy().Map(ca => ca.Sample());
                Stream<T> @out = new Stream<T>();
                MutableListener currentListener = new MutableListener();
                Action<Transaction, Cell<T>> h = (trans2, ca) =>
                {
                    currentListener.Unlisten();

                    currentListener.SetListener(ca.Value(trans2).Listen(@out.Node, trans2, @out.Send, false));
                };
                IListener l1 = cca.Value(trans1).Listen(@out.Node, trans1, h, false);
                return @out.UnsafeAttachListener(l1).UnsafeAttachListener(currentListener).HoldLazyInternal(za);
            }, false);
        }

        /// <summary>
        ///     Unwrap a discrete cell inside a cell to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the discrete cell.</typeparam>
        /// <param name="cca">The cell containing a discrete cell.</param>
        /// <returns>The unwrapped discrete cell.</returns>
        public static DiscreteCell<T> SwitchC<T>(this Cell<DiscreteCell<T>> cca) => new DiscreteCell<T>(cca.Map(c => c.Cell).SwitchC());

        /// <summary>
        ///     Unwrap a stream inside a cell to give a time-varying stream implementation.
        ///     When the cell changes value, the output stream will fire the simultaneous firing (if one exists) from the stream which the cell held at the beginning of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="csa">The cell containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        public static Stream<T> SwitchS<T>(this Cell<Stream<T>> csa)
        {
            return Transaction.Apply(trans1 =>
            {
                Stream<T> @out = new Stream<T>();
                MutableListener currentListener = new MutableListener();
                Action<Transaction, Stream<T>> hInitial = (trans2, sa) =>
                {
                    currentListener.Unlisten();

                    currentListener.SetListener(sa.Listen(@out.Node, trans2, @out.Send, false));
                };
                Action<Transaction, Stream<T>> h = (trans2, sa) =>
                {
                    trans2.Last(() =>
                    {
                        currentListener.Unlisten();

                        currentListener.SetListener(sa.Listen(@out.Node, trans2, @out.Send, true));
                    });
                };
                trans1.Prioritized(new Node<T>(), trans2 => hInitial(trans2, csa.SampleNoTransaction()));
                IListener l1 = csa.Updates(trans1).Listen(@out.Node, trans1, h, false);
                return @out.UnsafeAttachListener(l1).UnsafeAttachListener(currentListener);
            }, false);
        }

        /// <summary>
        ///     Unwrap a stream inside a cell to give a time-varying stream implementation.
        ///     When the cell changes value, the output stream will fire the simultaneous firing (if one exists) from the stream which the cell will hold at the end of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="csa">The cell containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        public static Stream<T> SwitchEarlyS<T>(this Cell<Stream<T>> csa)
        {
            return Transaction.Apply(trans1 =>
            {
                Stream<T> @out = new Stream<T>();
                Node<T> node = new Node<T>();
                Node<T>.Target nodeTarget = node.Link(trans1, (t, v) => { }, @out.Node).Item2;
                Guid listenerId;
                Action<Transaction, Tuple<T, Guid>, Guid> sendIfNodeTargetMatches = (t, v, i) =>
                {
                    if (v.Item2 == i)
                    {
                        @out.Send(t, v.Item1);
                    }
                };
                MutableListener currentListener = new MutableListener();
                Action<Transaction, Stream<T>> h = (trans2, sa) =>
                {
                    currentListener.Unlisten();

                    listenerId = Guid.NewGuid();
                    currentListener.SetListener(sa.Map(v => Tuple.Create(v, listenerId)).Listen(@out.Node, trans2, (t, v) => sendIfNodeTargetMatches(t, v, listenerId), false));
                };
                IListener l1 = csa.Value(trans1).Listen(node, trans1, h, false);
                return @out.UnsafeAttachListener(l1).UnsafeAttachListener(currentListener).UnsafeAttachListener(Listener.Create(node, nodeTarget));
            }, false);
        }

        /// <summary>
        ///     Lift a function into an enumerable of cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The enumerable of cells.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the function applied to the input cells' values.</returns>
        public static Cell<TResult> Lift<T, TResult>(this IEnumerable<Cell<T>> c, Func<IReadOnlyList<T>, TResult> f)
        {
            return c.ToArray().Lift(f);
        }

        /// <summary>
        ///     Lift a function into a collection of cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The collection of cells.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the function applied to the input cells' values.</returns>
        public static Cell<TResult> Lift<T, TResult>(this IReadOnlyCollection<Cell<T>> c, Func<IReadOnlyList<T>, TResult> f)
        {
            return Transaction.Apply(trans1 =>
            {
                Lazy<T[]> values = new Lazy<T[]>(() => c.Select(cell => cell.SampleNoTransaction()).ToArray());
                Stream<Action> @out = new Stream<Action>();
                Lazy<TResult> initialValue = new Lazy<TResult>(() => f(values.Value.ToArray()));
                IReadOnlyList<IListener> listeners = c.Select((cell, i) => cell.Updates(trans1).Listen(@out.Node, trans1,
                    (trans2, v) => @out.Send(trans2, () => values.Value[i] = v), false)).ToArray();
                return @out.Coalesce(trans1, (x, y) => x + y).Map(a =>
                {
                    a();
                    return f(values.Value.ToArray());
                }).UnsafeAttachListener(new CompositeListener(listeners)).HoldLazyInternal(initialValue);
            }, false);
        }

        /// <summary>
        ///     Lift into an enumerable of cells, so the returned cell always reflects a list of the input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <param name="c">The enumerable of cells.</param>
        /// <returns>A cell containing a list of the input cells' values.</returns>
        public static Cell<IReadOnlyList<T>> Lift<T>(this IEnumerable<Cell<T>> c)
        {
            return c.ToArray().Lift();
        }

        /// <summary>
        ///     Lift into a collection of cells, so the returned cell always reflects a list of the input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <param name="c">The collection of cells.</param>
        /// <returns>A cell containing a list of the input cells' values.</returns>
        public static Cell<IReadOnlyList<T>> Lift<T>(this IReadOnlyCollection<Cell<T>> c)
        {
            return c.Lift(v => (IReadOnlyList<T>)v.ToArray());
        }
    }
}