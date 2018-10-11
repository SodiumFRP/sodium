using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public static class BehaviorExtensionMethods
    {
        /// <summary>
        ///     Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <param name="bba">The behavior containing another behavior.</param>
        /// <returns>The unwrapped behavior.</returns>
        public static Behavior<T> SwitchC<T>(this Behavior<Behavior<T>> bba)
        {
            return Transaction.Apply(
                (trans1, _) =>
                {
                    Lazy<T> za = bba.SampleLazy().Map(ba => ba.Sample());
                    Stream<T> @out = new Stream<T>(bba.KeepListenersAlive);
                    MutableListener currentListener = new MutableListener();

                    void H(Transaction trans2, Behavior<T> ba)
                    {
                        currentListener.Unlisten();

                        currentListener.SetListener(ba.Value(trans2).Listen(@out.Node, trans2, @out.Send, false));
                    }

                    IListener l1 = bba.Value(trans1).Listen(@out.Node, trans1, H, false);
                    return @out.UnsafeAttachListener(l1)
                        .UnsafeAttachListener(currentListener)
                        .HoldLazyInternal(trans1, za);
                },
                false);
        }

        /// <summary>
        ///     Unwrap a cell inside a behavior to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="bca">The behavior containing a cell.</param>
        /// <returns>The unwrapped cell.</returns>
        public static Cell<T> SwitchC<T>(this Behavior<Cell<T>> bca) =>
            new Cell<T>(bca.Map(c => c.AsBehavior()).SwitchC());

        /// <summary>
        ///     Unwrap a stream inside a behavior to give a time-varying stream implementation.
        ///     When the behavior changes value, the output stream will fire the simultaneous firing (if one exists) from the stream
        ///     which the behavior held at the beginning of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="bsa">The behavior containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        public static Stream<T> SwitchS<T>(this Behavior<Stream<T>> bsa)
        {
            return Transaction.Apply(
                (trans1, _) =>
                {
                    Stream<T> @out = new Stream<T>(bsa.KeepListenersAlive);
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

                    trans1.Prioritized(new Node<T>(), trans2 => HInitial(trans2, bsa.SampleNoTransaction()));
                    IListener l1 = bsa.Updates().Listen(new Node<T>(), trans1, H, false);
                    return @out.UnsafeAttachListener(l1).UnsafeAttachListener(currentListener);
                },
                false);
        }

        /// <summary>
        ///     Lift a function into an enumerable of behaviors, so the returned behavior always reflects the specified function applied to
        ///     the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behaviors.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="b">The enumerable of behaviors.</param>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <returns>A behavior containing values resulting from the function applied to the input behaviors' values.</returns>
        public static Behavior<TResult> Lift<T, TResult>(
            this IEnumerable<Behavior<T>> b,
            Func<IReadOnlyList<T>, TResult> f) =>
            b.ToArray().Lift(f);

        /// <summary>
        ///     Lift a function into a collection of behaviors, so the returned behavior always reflects the specified function applied to
        ///     the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behaviors.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="b">The collection of behaviors.</param>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <returns>A behavior containing values resulting from the function applied to the input behaviors' values.</returns>
        public static Behavior<TResult> Lift<T, TResult>(
            this IReadOnlyCollection<Behavior<T>> b,
            Func<IReadOnlyList<T>, TResult> f)
        {
            return Transaction.Apply(
                (trans1, _) =>
                {
                    Stream<Action<T[]>> @out = new Stream<Action<T[]>>(
                        new FanOutKeepListenersAlive(b.Select(behavior => behavior.KeepListenersAlive)));
                    Lazy<TResult> initialValue =
                        new Lazy<TResult>(() => f(b.Select(behavior => behavior.SampleNoTransaction()).ToArray()));
                    IReadOnlyList<IListener> listeners = b.Select(
                            (behavior, i) => behavior.Updates()
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
                                T[] values = b.Select(behavior => behavior.SampleNoTransaction()).ToArray();
                                a(values);
                                return f(values);
                            })
                        .UnsafeAttachListener(new CompositeListener(listeners))
                        .HoldLazyInternal(trans1, initialValue);
                },
                false);
        }

        /// <summary>
        ///     Lift into an enumerable of behaviors, so the returned behavior always reflects a list of the input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behaviors.</typeparam>
        /// <param name="b">The enumerable of behaviors.</param>
        /// <returns>A behavior containing a list of the input behaviors' values.</returns>
        public static Behavior<IReadOnlyList<T>> Lift<T>(this IEnumerable<Behavior<T>> b) => b.ToArray().Lift();

        /// <summary>
        ///     Lift into a collection of behaviors, so the returned behavior always reflects a list of the input behaviors' values.
        /// </summary>
        /// <typeparam name="T">The type of the behaviors.</typeparam>
        /// <param name="b">The collection of behaviors.</param>
        /// <returns>A behavior containing a list of the input behaviors' values.</returns>
        public static Behavior<IReadOnlyList<T>> Lift<T>(this IReadOnlyCollection<Behavior<T>> b) =>
            b.Lift(v => v);

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