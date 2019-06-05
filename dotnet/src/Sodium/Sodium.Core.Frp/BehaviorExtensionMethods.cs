using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium.Frp
{
    internal static class BehaviorExtensionMethodsInternal
    {
        internal static Behavior<T> SwitchBImpl<T, T2>(this Behavior<T2> bba)
            where T2 : Behavior<T>
        {
            return TransactionInternal.Apply(
                (trans1, _) =>
                {
                    Lazy<T> za = bba.SampleLazyImpl().MapImpl(ba => ba.SampleImpl());
                    Stream<T> @out = new Stream<T>(bba.KeepListenersAlive);
                    MutableListener currentListener = new MutableListener();

                    void H(TransactionInternal trans2, Behavior<T> ba)
                    {
                        IListener cl = currentListener;
                        cl.Unlisten();

                        currentListener.SetListenerImpl(ba.Value(trans2).Listen(@out.Node, trans2, @out.Send, false));
                    }

                    IListener l1 = bba.Value(trans1).Listen(@out.Node, trans1, H, false);
                    return @out.UnsafeAttachListener(l1)
                        .UnsafeAttachListener(currentListener)
                        .HoldLazyInternal(trans1, za);
                },
                false);
        }

        internal static Cell<T> SwitchCImpl<T, T2>(this Behavior<T2> bca) where T2 : Cell<T> =>
            new Cell<T>(bca.MapImpl(c => c.BehaviorImpl).SwitchBImpl<T, Behavior<T>>());

        internal static Stream<T> SwitchSImpl<T, T2>(this Behavior<T2> bsa) where T2 : Stream<T>
        {
            return TransactionInternal.Apply(
                (trans1, _) =>
                {
                    Stream<T> @out = new Stream<T>(bsa.KeepListenersAlive);
                    MutableListener currentListener = new MutableListener();

                    void HInitial(TransactionInternal trans2, Stream<T> sa)
                    {
                        IListener cl = currentListener;
                        cl.Unlisten();

                        currentListener.SetListenerImpl(sa.Listen(@out.Node, trans2, @out.Send, false));
                    }

                    void H(TransactionInternal trans2, Stream<T> sa)
                    {
                        trans2.Last(
                            () =>
                            {
                                IListener cl = currentListener;
                                cl.Unlisten();

                                currentListener.SetListenerImpl(sa.Listen(@out.Node, trans2, @out.Send, true));
                            });
                    }

                    trans1.Prioritized(new Node<T>(), trans2 => HInitial(trans2, bsa.SampleNoTransaction()));
                    IListener l1 = bsa.Updates().Listen(new Node<T>(), trans1, H, false);
                    return @out.UnsafeAttachListener(l1).UnsafeAttachListener(currentListener);
                },
                false);
        }

        internal static Behavior<TResult> LiftBehaviorsImpl<T, T2, TResult>(
            this IEnumerable<T2> b,
            Func<IReadOnlyList<T>, TResult> f)
            where T2 : Behavior<T> =>
            b.ToArray().LiftBehaviorsImpl(f);

        internal static Behavior<TResult> LiftBehaviorsImpl<T, T2, TResult>(
            this IReadOnlyCollection<T2> b,
            Func<IReadOnlyList<T>, TResult> f)
            where T2 : Behavior<T>
        {
            return TransactionInternal.Apply(
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
                        .MapImpl(
                            a =>
                            {
                                T[] values = b.Select(behavior => behavior.SampleNoTransaction()).ToArray();
                                a(values);
                                return f(values);
                            })
                        .UnsafeAttachListener(ListenerInternal.CreateCompositeImpl(listeners))
                        .HoldLazyInternal(trans1, initialValue);
                },
                false);
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