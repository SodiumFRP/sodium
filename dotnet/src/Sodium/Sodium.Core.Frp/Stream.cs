using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium.Frp
{
    internal static class StreamInternal
    {
        internal static Stream<T> NeverImpl<T>() => new Stream<T>();

        internal static StreamSink<T> CreateSinkImpl<T>() => new StreamSink<T>();

        internal static StreamSink<T> CreateSinkImpl<T>(Func<T, T, T> coalesce) => new StreamSink<T>(coalesce);
    }

    /// <summary>
    ///     Represents a stream of discrete events/firings.
    /// </summary>
    /// <typeparam name="T">The type of values fired by the stream.</typeparam>
    public class Stream<T>
    {
        private readonly Guid streamId;
        internal readonly Node<T> Node;

        // ReSharper disable once CollectionNeverQueried.Local
        private readonly List<IListener> attachedListeners;

        private readonly StreamListenerManager.StreamListeners trackedListeners;
        private readonly List<T> firings;
        internal readonly IKeepListenersAlive KeepListenersAlive;

        private readonly object attachListenerLock = new object();

        internal Stream()
            : this(new KeepListenersAliveImplementation())
        {
        }

        internal Stream(IKeepListenersAlive keepListenersAlive)
        {
            this.streamId = Guid.NewGuid();
            this.KeepListenersAlive = keepListenersAlive;
            this.Node = new Node<T>();
            this.attachedListeners = new List<IListener>();
            this.trackedListeners = new StreamListenerManager.StreamListeners(this.streamId);
            this.firings = new List<T>();
        }

        internal IStrongListener ListenImpl(Action<T> handler)
        {
            IWeakListener innerListener = this.ListenWeakImpl(handler);
            StrongListener listener = null;
            listener = new StrongListener(
                () =>
                {
                    innerListener.Unlisten();

                    // ReSharper disable AccessToModifiedClosure
                    if (listener != null)
                    {
                        lock (this.KeepListenersAlive)
                        {
                            this.KeepListenersAlive.StopKeepingListenerAlive(listener);
                        }
                    }
                    // ReSharper restore AccessToModifiedClosure
                },
                innerListener);

            lock (this.KeepListenersAlive)
            {
                this.KeepListenersAlive.KeepListenerAlive(listener);
            }

            return listener;
        }

        internal IWeakListener ListenWeakImpl(Action<T> handler) => this.Listen(Node<T>.Null, (trans2, a) => handler(a));

        internal Stream<T> AttachListenerImpl(IListener listener)
        {
            lock (this.attachListenerLock)
            {
                return this.UnsafeAttachListener(listener);
            }
        }

        internal IStrongListener ListenOnceImpl(Action<T> handler)
        {
            IStrongListener listener = null;
            bool unlistenEarly = false;
            listener = this.ListenImpl(
                a =>
                {
                    // ReSharper disable once AccessToModifiedClosure
                    if (listener == null)
                    {
                        unlistenEarly = true;
                    }
                    else
                    {
                        // ReSharper disable once AccessToModifiedClosure
                        listener.Unlisten();
                        listener = null;
                    }

                    handler(a);
                });
            if (unlistenEarly)
            {
                listener.Unlisten();
                listener = null;
            }
            return listener;
        }

        internal IWeakListener Listen(Node target, Action<TransactionInternal, T> action) => TransactionInternal.Apply(
            (trans1, _) => this.Listen(target, trans1, action, false),
            false);

        internal IWeakListener Listen(
            Node target,
            TransactionInternal trans,
            Action<TransactionInternal, T> action,
            bool suppressEarlierFirings)
        {
            (bool changed, Node<T>.Target nodeTarget) = this.Node.Link(trans, action, target);
            if (changed)
            {
                trans.SetNeedsRegenerating();
            }

            // ReSharper disable once LocalVariableHidesMember
            List<T> firings = this.firings.ToList();
            if (!suppressEarlierFirings && firings.Count > 0)
            {
                trans.Prioritized(
                    target,
                    trans2 =>
                    {
                        // Anything sent already in this transaction must be sent now so that
                        // there's no order dependency between send and listen.
                        foreach (T a in firings)
                        {
                            TransactionInternal.InCallback++;
                            try
                            {
                                // Don't allow transactions to interfere with Sodium
                                // internals.
                                action(trans2, a);
                            }
                            finally
                            {
                                TransactionInternal.InCallback--;
                            }
                        }
                    });
            }

            return new ListenerImplementation(this, action, nodeTarget);
        }

        internal Stream<TResult> MapImpl<TResult>(Func<T, TResult> f)
        {
            Stream<TResult> @out = new Stream<TResult>(this.KeepListenersAlive);
            IListener l = this.Listen(@out.Node, (trans2, a) => @out.Send(trans2, f(a)));
            return @out.UnsafeAttachListener(l);
        }

        internal Stream<TResult> MapToImpl<TResult>(TResult value) => this.MapImpl(_ => value);

        internal Cell<T> HoldImpl(T initialValue) => new Cell<T>(this.HoldInternal(initialValue));

        internal Behavior<T> HoldInternal(T initialValue) => new Behavior<T>(this, initialValue);

        internal Cell<T> HoldLazyImpl(Lazy<T> initialValue) =>
            TransactionInternal.Apply((trans, _) => new Cell<T>(this.HoldLazyInternal(trans, initialValue)), false);

        internal Behavior<T> HoldLazyInternal(TransactionInternal trans, Lazy<T> initialValue) =>
            new LazyBehavior<T>(trans, this, initialValue);

        internal Stream<TResult> SnapshotImpl<TResult>(Cell<TResult> c) => this.SnapshotImpl(c.BehaviorImpl);

        internal Stream<TResult> SnapshotImpl<TResult>(Behavior<TResult> b) => this.SnapshotImpl(b, (_, a) => a);

        internal Stream<TResult> SnapshotImpl<T1, TResult>(Cell<T1> c, Func<T, T1, TResult> f) =>
            this.SnapshotImpl(c.BehaviorImpl, f);

        internal Stream<TResult> SnapshotImpl<T1, TResult>(Behavior<T1> b, Func<T, T1, TResult> f)
        {
            Stream<TResult> @out = new Stream<TResult>(this.KeepListenersAlive);
            IListener l = this.Listen(@out.Node, (trans2, a) => @out.Send(trans2, f(a, b.SampleNoTransaction())));
            return @out.UnsafeAttachListener(l);
        }

        internal Stream<TResult> SnapshotImpl<T1, T2, TResult>(
            Cell<T1> c1,
            Cell<T2> c2,
            Func<T, T1, T2, TResult> f) => this.SnapshotImpl(c1.BehaviorImpl, c2.BehaviorImpl, f);

        internal Stream<TResult> SnapshotImpl<T1, T2, TResult>(Behavior<T1> b1, Behavior<T2> b2, Func<T, T1, T2, TResult> f)
        {
            Stream<TResult> @out = new Stream<TResult>(this.KeepListenersAlive);
            IListener l = this.Listen(
                @out.Node,
                (trans2, a) => @out.Send(trans2, f(a, b1.SampleNoTransaction(), b2.SampleNoTransaction())));
            return @out.UnsafeAttachListener(l);
        }

        internal Stream<TResult> SnapshotImpl<T1, T2, T3, TResult>(
            Cell<T1> c1,
            Cell<T2> c2,
            Cell<T3> c3,
            Func<T, T1, T2, T3, TResult> f) => this.SnapshotImpl(c1.BehaviorImpl, c2.BehaviorImpl, c3.BehaviorImpl, f);

        internal Stream<TResult> SnapshotImpl<T1, T2, T3, TResult>(
            Behavior<T1> b1,
            Behavior<T2> b2,
            Behavior<T3> b3,
            Func<T, T1, T2, T3, TResult> f)
        {
            Stream<TResult> @out = new Stream<TResult>(this.KeepListenersAlive);
            IListener l = this.Listen(
                @out.Node,
                (trans2, a) => @out.Send(
                    trans2,
                    f(a, b1.SampleNoTransaction(), b2.SampleNoTransaction(), b3.SampleNoTransaction())));
            return @out.UnsafeAttachListener(l);
        }

        internal Stream<TResult> SnapshotImpl<T1, T2, T3, T4, TResult>(
            Cell<T1> c1,
            Cell<T2> c2,
            Cell<T3> c3,
            Cell<T4> c4,
            Func<T, T1, T2, T3, T4, TResult> f) => this.SnapshotImpl(c1.BehaviorImpl, c2.BehaviorImpl, c3.BehaviorImpl, c4.BehaviorImpl, f);

        internal Stream<TResult> SnapshotImpl<T1, T2, T3, T4, TResult>(
            Behavior<T1> b1,
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Func<T, T1, T2, T3, T4, TResult> f)
        {
            Stream<TResult> @out = new Stream<TResult>(this.KeepListenersAlive);
            IListener l = this.Listen(
                @out.Node,
                (trans2, a) => @out.Send(
                    trans2,
                    f(
                        a,
                        b1.SampleNoTransaction(),
                        b2.SampleNoTransaction(),
                        b3.SampleNoTransaction(),
                        b4.SampleNoTransaction())));
            return @out.UnsafeAttachListener(l);
        }

        internal Stream<T> OrElseImpl(Stream<T> s) => this.MergeImpl(s, (left, right) => left);

        private Stream<T> Merge(TransactionInternal trans, Stream<T> s)
        {
            Stream<T> @out = new Stream<T>(this.KeepListenersAlive);
            Node<T> left = new Node<T>();
            Node<T> right = @out.Node;
            (bool changed, Node<T>.Target nodeTarget) = left.Link(trans, (t, v) => { }, right);
            if (changed)
            {
                trans.SetNeedsRegenerating();
            }

            Action<TransactionInternal, T> h = @out.Send;
            IListener l1 = this.Listen(left, h);
            IListener l2 = s.Listen(right, h);
            return @out.UnsafeAttachListener(l1)
                .UnsafeAttachListener(l2)
                .UnsafeAttachListener(ListenerInternal.CreateFromNodeAndTarget(left, nodeTarget));
        }

        internal Stream<T> MergeImpl(Stream<T> s, Func<T, T, T> f) => TransactionInternal.Apply((trans, _) => this.Merge(trans, s, f), false);

        internal Stream<T> Merge(TransactionInternal trans, Stream<T> s, Func<T, T, T> f) =>
            this.Merge(trans, s).Coalesce(trans, f);

        internal Stream<T> Coalesce(TransactionInternal trans1, Func<T, T, T> f)
        {
            Stream<T> @out = new Stream<T>(this.KeepListenersAlive);
            Action<TransactionInternal, T> h = CoalesceHandler.Create(f, @out);
            IListener l = this.Listen(@out.Node, trans1, h, false);
            return @out.UnsafeAttachListener(l);
        }

        /// <summary>
        ///     Clean up the output by discarding any firing other than the last one.
        /// </summary>
        /// <param name="trans">The transaction to get the last firing from.</param>
        /// <returns>A stream containing only the last event firing from the specified transaction.</returns>
        internal Stream<T> LastFiringOnly(TransactionInternal trans) => this.Coalesce(trans, (first, second) => second);

        internal Stream<T> FilterImpl(Func<T, bool> predicate)
        {
            Stream<T> @out = new Stream<T>(this.KeepListenersAlive);
            IListener l = this.Listen(
                @out.Node,
                (trans2, a) =>
                {
                    if (predicate(a))
                    {
                        @out.Send(trans2, a);
                    }
                });
            return @out.UnsafeAttachListener(l);
        }

        internal Stream<T> GateImpl(Cell<bool> c) => this.GateImpl(c.BehaviorImpl);

        internal Stream<T> GateImpl(Behavior<bool> b) => this.SnapshotImpl(b, (a, pred) => pred ? MaybeInternal.Some(a) : MaybeInternal.None).FilterMaybeInternal();

        internal Stream<T> CalmImpl(Func<T, T, bool> areEqual) =>
            this.Calm(new Lazy<MaybeInternal<T>>(() => MaybeInternal.None), areEqual);

        internal Stream<T> Calm(Lazy<MaybeInternal<T>> init, Func<T, T, bool> areEqual)
        {
            return this.CollectLazyImpl(
                    init,
                    (a, lastA) =>
                    {
                        if (lastA.Match(v => areEqual(v, a), () => false))
                        {
                            return (ReturnValue: MaybeInternal.None, State: lastA);
                        }

                        MaybeInternal<T> ma = MaybeInternal.Some(a);
                        return (ReturnValue: ma, State: ma);
                    })
                .FilterMaybeInternal();
        }

        internal Stream<TReturn> CollectImpl<TState, TReturn>(
            TState initialState,
            Func<T, TState, (TReturn ReturnValue, TState State)> f) =>
            this.CollectLazyImpl(new Lazy<TState>(() => initialState), f);

        internal Stream<TReturn> CollectLazyImpl<TState, TReturn>(
            Lazy<TState> initialState,
            Func<T, TState, (TReturn ReturnValue, TState State)> f)
        {
            return TransactionInternal.Apply(
                (trans, _) =>
                {
                    LoopedStream<TState> es = new LoopedStream<TState>();
                    Behavior<TState> s = es.HoldLazyInternal(trans, initialState);
                    Stream<(TReturn ReturnValue, TState State)> ebs = this.SnapshotImpl(s, f);
                    Stream<TReturn> eb = ebs.MapImpl(bs => bs.ReturnValue);
                    Stream<TState> esOut = ebs.MapImpl(bs => bs.State);
                    es.Loop(trans, esOut);
                    return eb;
                },
                false);
        }

        internal Cell<TReturn> AccumImpl<TReturn>(TReturn initialState, Func<T, TReturn, TReturn> f) =>
            this.AccumLazyImpl(new Lazy<TReturn>(() => initialState), f);

        internal Cell<TReturn> AccumLazyImpl<TReturn>(Lazy<TReturn> initialState, Func<T, TReturn, TReturn> f)
        {
            return TransactionInternal.Apply(
                (trans, _) =>
                {
                    LoopedStream<TReturn> es = new LoopedStream<TReturn>();
                    Behavior<TReturn> s = es.HoldLazyInternal(trans, initialState);
                    Stream<TReturn> esOut = this.SnapshotImpl(s, f);
                    es.Loop(trans, esOut);
                    return esOut.HoldLazyImpl(initialState);
                },
                false);
        }

        internal Stream<T> OnceImpl()
        {
            // This is a bit long-winded but it's efficient because it unregisters
            // the listener.
            Stream<T> @out = new Stream<T>(this.KeepListenersAlive);
            IListener listener = null;
            bool unlistenEarly = false;
            listener = this.Listen(
                @out.Node,
                (trans, a) =>
                {
                    // ReSharper disable AccessToModifiedClosure
                    if (listener != null)
                    {
                        @out.Send(trans, a);

                        // ReSharper disable once AccessToModifiedClosure
                        if (listener == null)
                        {
                            unlistenEarly = true;
                        }
                        else
                        {
                            // ReSharper disable once AccessToModifiedClosure
                            listener.Unlisten();
                            listener = null;
                        }
                    }
                    // ReSharper restore AccessToModifiedClosure
                });
            if (unlistenEarly)
            {
                listener.Unlisten();
                listener = null;
            }

            return @out.UnsafeAttachListener(listener);
        }

        // This is not thread-safe, so one of these two conditions must apply:
        // 1. We are within a transaction, since in the current implementation
        //    a transaction locks out all other threads.
        // 2. The object on which this is being called was created has not yet
        //    been returned from the method where it was created, so it can't
        //    be shared between threads.
        internal Stream<T> UnsafeAttachListener(IListener cleanup)
        {
            this.attachedListeners.Add(cleanup);
            this.trackedListeners.AddListener(cleanup.GetListenerWithWeakReference());
            return this;
        }

        internal void Send(TransactionInternal trans, T a)
        {
            if (this.firings.Count < 1)
            {
                trans.Last(this.firings.Clear);
            }

            this.firings.Add(a);

            foreach (Node<T>.Target target in this.Node.GetListenersCopy())
            {
                trans.Prioritized(
                    target.Node,
                    trans2 =>
                    {
                        TransactionInternal.InCallback++;
                        try
                        {
                            // Don't allow transactions to interfere with Sodium
                            // internals.
                            // Dereference the weak reference
                            if (target.Action.TryGetTarget(out Action<TransactionInternal, T> action))
                            {
                                // If it hasn't been garbage collected, call it.
                                if (target.IsActivated)
                                {
                                    action(trans2, a);
                                }
                            }
                            else
                            {
                                // If it has been garbage collected, remove it.
                                this.Node.RemoveListener(target);
                            }
                        }
                        finally
                        {
                            TransactionInternal.InCallback--;
                        }
                    });
            }
        }

        ~Stream() => StreamListenerManager.Remove(this.streamId);

        private class StrongListener : IStrongListener
        {
            private readonly Action unlisten;
            private readonly IListener listener;

            public StrongListener(Action unlisten, IListener listener)
            {
                this.unlisten = unlisten;
                this.listener = listener;
            }

            public void Unlisten() => this.unlisten();

            public IListenerWithWeakReference GetListenerWithWeakReference() =>
                this.listener.GetListenerWithWeakReference();

            public void Dispose() => this.Unlisten();
        }

        private class ListenerImplementation : IWeakListener
        {
            // It's essential that we keep the action alive, since the node uses
            // a weak reference.
            // ReSharper disable once NotAccessedField.Local
            private readonly Action<TransactionInternal, T> action;

            // It's essential that we keep the listener alive while the caller holds
            // the Listener, so that the garbage collector doesn't get triggered.
            // ReSharper disable once NotAccessedField.Local
            private readonly Stream<T> stream;

            private readonly WeakListener weakListener;

            public ListenerImplementation(Stream<T> stream, Action<TransactionInternal, T> action, Node<T>.Target target)
            {
                this.stream = stream;
                this.action = action;

                this.weakListener = new WeakListener(stream?.Node, target);
            }

            public void Unlisten()
            {
                this.weakListener.Unlisten();
            }

            public IListenerWithWeakReference GetListenerWithWeakReference() => this.weakListener;
        }

        private class WeakListener : IListenerWithWeakReference
        {
            private readonly Node<T> node;
            private readonly Node<T>.Target target;

            public WeakListener(Node<T> node, Node<T>.Target target)
            {
                this.node = node;
                this.target = target;
            }

            public void Unlisten()
            {
                this.node?.Unlink(this.target);
            }
        }

        private class KeepListenersAliveImplementation : IKeepListenersAlive
        {
            private readonly HashSet<IListener> listeners = new HashSet<IListener>();

            // ReSharper disable once CollectionNeverQueried.Local
            private readonly List<IKeepListenersAlive> childKeepListenersAliveList = new List<IKeepListenersAlive>();

            public void KeepListenerAlive(IListener listener)
            {
                this.listeners.Add(listener);
            }

            public void StopKeepingListenerAlive(IListener listener)
            {
                this.listeners.Remove(listener);
            }

            public void Use(IKeepListenersAlive childKeepListenersAlive)
            {
                this.childKeepListenersAliveList.Add(childKeepListenersAlive);
            }
        }
    }
}