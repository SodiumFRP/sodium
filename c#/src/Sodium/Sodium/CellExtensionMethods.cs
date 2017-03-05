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
                IListener currentListener = null;
                Action<Transaction, Stream<T>> hInitial = (trans2, sa) =>
                {
                    using (currentListener)
                    {
                    }

                    currentListener = sa.Listen(@out.Node, trans2, @out.Send, false);
                };
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
                trans1.Prioritized(new Node<T>(0), trans2 => hInitial(trans2, csa.SampleNoTransaction()));
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
                Guid listenerId;
                Action<Transaction, Tuple<T, Guid>, Guid> sendIfNodeTargetMatches = (t, v, i) =>
                {
                    if (v.Item2 == i)
                    {
                        @out.Send(t, v.Item1);
                    }
                };
                IListener currentListener = null;
                Action<Transaction, Stream<T>> h = (trans2, sa) =>
                {
                    using (currentListener)
                    {
                    }

                    listenerId = Guid.NewGuid();
                    currentListener = sa.Map(v => Tuple.Create(v, listenerId)).Listen(@out.Node, trans2, (t, v) => sendIfNodeTargetMatches(t, v, listenerId), false);
                };
                IListener l1 = csa.Value(trans1).Listen(node, trans1, h, false);
                return @out.UnsafeAddCleanup(l1).UnsafeAddCleanup(new Listener(() => node.Unlink(nodeTarget)));
            });
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
            return c.LiftToArray().Map<IReadOnlyList<T>>(v => v);
        }

        internal static Cell<T[]> LiftToArray<T>(this IEnumerable<Cell<T>> c)
        {
            return c.ToArray().LiftToArray();
        }

        internal static Cell<T[]> LiftToArray<T>(this IReadOnlyCollection<Cell<T>> c)
        {
            return Transaction.Apply(trans1 =>
            {
                T[] values = c.Select(cell => cell.SampleNoTransaction()).ToArray();
                Stream<Action> @out = new Stream<Action>(new FanOutKeepListenersAlive(c.Select(cell => cell.KeepListenersAlive)));
                Lazy<T[]> initialValue = new Lazy<T[]>(() => values.ToArray());
                IReadOnlyList<IListener> listeners = c.Select((cell, i) => cell.Updates(trans1).Listen(@out.Node, trans1,
                    (trans2, v) => @out.Send(trans2, () => values[i] = v), false)).ToArray();
                return @out.Coalesce(trans1, (x, y) => x + y).Map(a =>
                  {
                      a();
                      return values.ToArray();
                  }).UnsafeAddCleanup(new ImmutableCompositeListener(listeners)).HoldLazy(initialValue);
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