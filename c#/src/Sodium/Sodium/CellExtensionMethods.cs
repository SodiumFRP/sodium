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
                Stream<T> @out = new Stream<T>(cca.KeepListenersAlive);
                IListener currentListener = null;
                Action<Transaction, Cell<T>> h = (trans2, ca) =>
                {
                    using (currentListener)
                    {
                    }

                    currentListener = ca.Value(trans2).Listen(@out.Node, trans2, @out.Send, false);
                };
                IListener l1 = cca.Value(trans1).Listen(@out.Node, trans1, h, false);
                return @out.UnsafeAddCleanup(l1).HoldLazy(za);
            });
        }

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
                Stream<T> @out = new Stream<T>(csa.KeepListenersAlive);
                IListener currentListener = csa.SampleNoTransaction().Listen(@out.Node, trans1, @out.Send, false);
                Action<Transaction, Stream<T>> h = (trans2, sa) =>
                {
                    trans2.Last(() =>
                    {
                        using (currentListener)
                        {
                        }

                        currentListener = sa.Listen(@out.Node, trans2, @out.Send, true);
                    });
                };
                IListener l1 = csa.Updates(trans1).Listen(@out.Node, trans1, h, false);
                return @out.UnsafeAddCleanup(l1);
            });
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
                Stream<T> @out = new Stream<T>(csa.KeepListenersAlive);
                Node<T> node = new Node<T>(0);
                Node<T>.Target nodeTarget = node.Link((t, v) => { }, @out.Node).Item2;
                Guid listenerId = Guid.NewGuid();
                Action<Transaction, Tuple<T, Guid>, Guid> sendIfNodeTargetMatches = (t, v, i) =>
                {
                    if (v.Item2 == i)
                    {
                        @out.Send(t, v.Item1);
                    }
                };
                IListener currentListener = csa.SampleNoTransaction().Map(v => Tuple.Create(v, listenerId)).Listen(node, trans1, (t, v) => sendIfNodeTargetMatches(t, v, listenerId), false);
                Action<Transaction, Stream<T>> h = (trans2, sa) =>
                {
                    using (currentListener)
                    {
                    }

                    node.Unlink(nodeTarget);
                    nodeTarget = node.Link((t, v) => { }, @out.Node).Item2;
                    listenerId = Guid.NewGuid();
                    currentListener = sa.Map(v => Tuple.Create(v, listenerId)).Listen(@out.Node, trans2, (t, v) => sendIfNodeTargetMatches(t, v, listenerId), false);
                };
                IListener l1 = csa.Updates(trans1).Listen(node, trans1, h, false);
                return @out.UnsafeAddCleanup(l1).UnsafeAddCleanup(new Listener(() => node.Unlink(nodeTarget)));
            });
        }

        /// <summary>
        ///     Lift a function into cells, so the returned cell always reflects the specified function applied to the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The enumerable of cells.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the function applied to the input cells' values.</returns>
        public static Cell<TResult> Lift<T, TResult>(this IEnumerable<Cell<T>> c, Func<IReadOnlyList<T>, TResult> f)
        {
            return Transaction.Apply(trans1 =>
            {
                IReadOnlyList<Cell<T>> cells = c.ToArray();
                T[] values = cells.Select(cell => cell.SampleNoTransaction()).ToArray();
                Stream<TResult> @out = new Stream<TResult>(new FanOutKeepListenersAlive(cells.Select(cell => cell.KeepListenersAlive)));
                Lazy<TResult> initialValue = new Lazy<TResult>(() => f(values.ToArray()));
                IEnumerable<IListener> listeners = cells.Select((cell, i) => cell.Updates(trans1).Listen(@out.Node, trans1, (trans2, v) =>
                  {
                      values[i] = v;
                      @out.Send(trans2, f(values.ToArray()));
                  }, false));
                return @out.UnsafeAddCleanup(new ImmutableCompositeListener(listeners)).HoldLazy(initialValue);
            });
        }

        private class FanOutKeepListenersAlive : IKeepListenersAlive
        {
            private readonly IReadOnlyList<IKeepListenersAlive> keepListenersAliveList;

            public FanOutKeepListenersAlive(IEnumerable<IKeepListenersAlive> keepListenersAliveEnumerable)
            {
                this.keepListenersAliveList = keepListenersAliveEnumerable.ToArray();
            }

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