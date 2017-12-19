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
            return Transaction.Apply(
                trans1 =>
                {
                    Lazy<T> za = cca.SampleLazy().Map(ca => ca.Sample());
                    Stream<T> @out = new Stream<T>(cca.KeepListenersAlive);
                    MutableListener currentListener = new MutableListener();

                    void H(Transaction trans2, Cell<T> ca)
                    {
                        currentListener.Unlisten();

                        currentListener.SetListener(ca.Value(trans2).Listen(@out.Node, trans2, @out.Send, false));
                    }

                    IListener l1 = cca.Value(trans1).Listen(@out.Node, trans1, H, false);
                    return @out.UnsafeAttachListener(l1).UnsafeAttachListener(currentListener).HoldLazyInternal(za);
                },
                false);
        }

        /// <summary>
        ///     Unwrap a discrete cell inside a cell to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the discrete cell.</typeparam>
        /// <param name="cca">The cell containing a discrete cell.</param>
        /// <returns>The unwrapped discrete cell.</returns>
        public static DiscreteCell<T> SwitchC<T>(this Cell<DiscreteCell<T>> cca) =>
            new DiscreteCell<T>(cca.Map(c => c.Cell).SwitchC());

        /// <summary>
        ///     Unwrap a stream inside a cell to give a time-varying stream implementation.
        ///     When the cell changes value, the output stream will fire the simultaneous firing (if one exists) from the stream
        ///     which the cell held at the beginning of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="csa">The cell containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        public static Stream<T> SwitchS<T>(this Cell<Stream<T>> csa)
        {
            return Transaction.Apply(
                trans1 =>
                {
                    Stream<T> @out = new Stream<T>(csa.KeepListenersAlive);
                    MutableListener currentListener = new MutableListener();

                    void HInitial(Transaction trans2, Stream<T> sa)
                    {
                        currentListener.Unlisten();

                        currentListener.SetListener(sa.Listen(@out.Node, trans2, @out.Send, false));
                    }

                    void H(Transaction trans2, Stream<T> sa)
                    {
                        trans2.Last(
                            () =>
                            {
                                currentListener.Unlisten();

                                currentListener.SetListener(sa.Listen(@out.Node, trans2, @out.Send, true));
                            });
                    }

                    trans1.Prioritized(new Node<T>(), trans2 => HInitial(trans2, csa.SampleNoTransaction()));
                    IListener l1 = csa.Updates(trans1).Listen(new Node<T>(), trans1, H, false);
                    return @out.UnsafeAttachListener(l1).UnsafeAttachListener(currentListener);
                },
                false);
        }

        /// <summary>
        ///     Unwrap a stream inside a cell to give a time-varying stream implementation.
        ///     When the cell changes value, the output stream will fire the simultaneous firing (if one exists) from the stream
        ///     which the cell will hold at the end of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="csa">The cell containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        public static Stream<T> SwitchEarlyS<T>(this Cell<Stream<T>> csa)
        {
            return Transaction.Apply(
                trans1 =>
                {
                    Stream<T> @out = new Stream<T>(csa.KeepListenersAlive);
                    Node<T> node = new Node<T>();
                    (bool changed, Node<T>.Target nodeTarget) = node.Link(trans1, (t, v) => { }, @out.Node);
                    if (changed)
                    {
                        trans1.SetNeedsRegenerating();
                    }
                    Guid listenerId;

                    void SendIfNodeTargetMatches(Transaction t, (T Value, Guid ListenerId) v, Guid i)
                    {
                        if (v.ListenerId == i)
                        {
                            @out.Send(t, v.Value);
                        }
                    }

                    MutableListener currentListener = new MutableListener();

                    void Handler(Transaction trans2, Stream<T> sa)
                    {
                        currentListener.Unlisten();

                        listenerId = Guid.NewGuid();
                        currentListener.SetListener(
                            sa.Map(v => (Value: v, ListenerId: listenerId))
                                .Listen(@out.Node, trans2, (t, v) => SendIfNodeTargetMatches(t, v, listenerId), false));
                    }

                    IListener l1 = csa.Value(trans1).Listen(node, trans1, Handler, false);
                    return @out.UnsafeAttachListener(l1)
                        .UnsafeAttachListener(currentListener)
                        .UnsafeAttachListener(Listener.Create(node, nodeTarget));
                },
                false);
        }

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
        public static Cell<TResult> Lift<T, TResult>(this IEnumerable<Cell<T>> c, Func<IReadOnlyList<T>, TResult> f) =>
            c.ToArray().Lift(f);

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
        public static Cell<TResult> Lift<T, TResult>(
            this IReadOnlyCollection<Cell<T>> c,
            Func<IReadOnlyList<T>, TResult> f)
        {
            return Transaction.Apply(
                trans1 =>
                {
                    Stream<Action<T[]>> @out = new Stream<Action<T[]>>(
                        new FanOutKeepListenersAlive(c.Select(cell => cell.KeepListenersAlive)));
                    Lazy<TResult> initialValue =
                        new Lazy<TResult>(() => f(c.Select(cell => cell.SampleNoTransaction()).ToArray()));
                    IReadOnlyList<IListener> listeners = c.Select(
                            (cell, i) => cell.Updates(trans1)
                                .Listen(
                                    @out.Node,
                                    trans1,
                                    (trans2, v) => @out.Send(trans2, vv => vv[i] = v),
                                    false))
                        .ToArray();
                    return @out.Coalesce(trans1, (x, y) => x + y)
                        .Map(
                            a =>
                            {
                                T[] values = c.Select(cell => cell.SampleNoTransaction()).ToArray();
                                a(values);
                                return f(values);
                            })
                        .UnsafeAttachListener(new CompositeListener(listeners))
                        .HoldLazyInternal(initialValue);
                },
                false);
        }

        /// <summary>
        ///     Lift into an enumerable of cells, so the returned cell always reflects a list of the input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <param name="c">The enumerable of cells.</param>
        /// <returns>A cell containing a list of the input cells' values.</returns>
        public static Cell<IReadOnlyList<T>> Lift<T>(this IEnumerable<Cell<T>> c) => c.ToArray().Lift();

        /// <summary>
        ///     Lift into a collection of cells, so the returned cell always reflects a list of the input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <param name="c">The collection of cells.</param>
        /// <returns>A cell containing a list of the input cells' values.</returns>
        public static Cell<IReadOnlyList<T>> Lift<T>(this IReadOnlyCollection<Cell<T>> c)
        {
            return c.Lift(v => (IReadOnlyList<T>) v.ToArray());
        }

        private class FanOutKeepListenersAlive : IKeepListenersAlive
        {
            private readonly IReadOnlyList<IKeepListenersAlive> keepListenersAliveList;

            public FanOutKeepListenersAlive(IEnumerable<IKeepListenersAlive> keepListenersAliveEnumerable) =>
                this.keepListenersAliveList = keepListenersAliveEnumerable.ToArray();

            public void KeepListenerAlive(IListener listener)
            {
                foreach (IKeepListenersAlive keepListenersAlive in this.keepListenersAliveList)
                {
                    keepListenersAlive.KeepListenerAlive(listener);
                }
            }

            public void StopKeepingListenerAlive(IListener listener)
            {
                foreach (IKeepListenersAlive keepListenersAlive in this.keepListenersAliveList)
                {
                    keepListenersAlive.StopKeepingListenerAlive(listener);
                }
            }

            public void Use(IKeepListenersAlive childKeepListenersAlive)
            {
                foreach (IKeepListenersAlive keepListenersAlive in this.keepListenersAliveList)
                {
                    keepListenersAlive.Use(childKeepListenersAlive);
                }
            }
        }
    }
}