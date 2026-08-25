using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

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
        internal readonly Node<T> Node;

        // Everything below is allocated on first use. Streams are created in bulk - a single
        // two-cell Lift builds around twenty of them - and a stream that is only ever an
        // intermediate step in a chain never sends, never has a listener attached, and never has
        // AttachListener called on it, so eagerly allocating for all three was most of what a
        // stream cost to construct.

        // ReSharper disable once CollectionNeverQueried.Local
        private List<IListener> attachedListeners;

        private readonly StreamListenerManager.StreamListeners trackedListeners;
        private List<T> firings;

        // Cached alongside firings because a method group conversion allocates a fresh delegate
        // each time, and Send hands this to trans.Last on the first firing of every transaction.
        private Action clearFirings;
        internal readonly IKeepListenersAlive KeepListenersAlive;

        private object attachListenerLock;

        internal Stream()
            : this(new KeepListenersAliveImplementation())
        {
        }

        internal Stream(IKeepListenersAlive keepListenersAlive)
        {
            this.KeepListenersAlive = keepListenersAlive;
            this.Node = new Node<T>();

            // Last, so nothing half-built is reachable from the registry. The registry only ever
            // holds this stream through a weak handle, so registering here does not keep it alive.
            this.trackedListeners = new StreamListenerManager.StreamListeners(this);
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
            lock (this.AttachListenerLock)
            {
                return this.UnsafeAttachListener(listener);
            }
        }

        // Created on demand like the rest, but via CompareExchange rather than a plain null check,
        // since there is no other lock available to guard creating this one.
        private object AttachListenerLock
        {
            get
            {
                object existing = this.attachListenerLock;
                if (existing != null)
                {
                    return existing;
                }

                Interlocked.CompareExchange(ref this.attachListenerLock, new object(), null);
                return this.attachListenerLock;
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
            (trans1, _) => this.Listen(target, trans1, action, false));

        internal IWeakListener Listen(
            Node target,
            TransactionInternal trans,
            Action<TransactionInternal, T> action,
            bool suppressEarlierFirings)
        {
            Node<T>.Target nodeTarget = this.Node.Link(trans, action, target);

            // Only snapshot the firings when they are actually going to be replayed - the copy
            // used to be taken unconditionally, on every listen, including the overwhelmingly
            // common case of a stream that has not fired in this transaction.
            if (!suppressEarlierFirings && this.firings != null && this.firings.Count > 0)
            {
                // ReSharper disable once LocalVariableHidesMember
                List<T> firings = this.firings.ToList();

                trans.Prioritized(
                    target,
                    trans2 =>
                    {
                        // Anything sent already in this transaction must be sent now so that
                        // there's no order dependency between send and listen.
                        foreach (T a in firings)
                        {
                            trans2.InCallback++;
                            try
                            {
                                // Don't allow transactions to interfere with Sodium
                                // internals.
                                action(trans2, a);
                            }
                            finally
                            {
                                trans2.InCallback--;
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
            TransactionInternal.Apply((trans, _) => new Cell<T>(this.HoldLazyInternal(trans, initialValue)));

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
            Node<T>.Target nodeTarget = left.Link(trans, (t, v) => { }, right);

            Action<TransactionInternal, T> h = @out.Send;
            IListener l1 = this.Listen(left, h);
            IListener l2 = s.Listen(right, h);
            return @out.UnsafeAttachListener(l1)
                .UnsafeAttachListener(l2)
                .UnsafeAttachListener(ListenerInternal.CreateFromNodeAndTarget(left, nodeTarget));
        }

        internal Stream<T> MergeImpl(Stream<T> s, Func<T, T, T> f) => TransactionInternal.Apply((trans, _) => this.Merge(trans, s, f));

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

        /// <summary>
        ///     Suppresses firings equal to the last one that got through.
        /// </summary>
        /// <remarks>
        ///     Written directly rather than on top of CollectLazyImpl. Going through Collect meant a
        ///     looped stream, a behavior to hold the state, a snapshot and two maps, plus a filter on
        ///     the way out - six streams to remember one value.
        ///
        ///     The state is kept in two fields rather than one, which is what Collect got for free by
        ///     holding it in a behavior: a snapshot reads a behavior with SampleNoTransaction, so every
        ///     firing within a transaction compared against the value the behavior had when that
        ///     transaction opened, and the behavior then committed whatever the final firing produced.
        ///     A single field updated in place would instead let an earlier firing in the same
        ///     transaction be seen by a later one, which is a different stream.
        /// </remarks>
        internal Stream<T> Calm(Lazy<MaybeInternal<T>> init, Func<T, T, bool> areEqual) =>
            TransactionInternal.Apply(
                (trans1, _) =>
                {
                    Stream<T> @out = new Stream<T>(this.KeepListenersAlive);

                    MaybeInternal<T> committed = MaybeInternal.None;
                    bool committedIsSet = false;
                    MaybeInternal<T> pending = MaybeInternal.None;
                    bool hasPending = false;

                    void EnsureCommittedIsSet()
                    {
                        if (!committedIsSet)
                        {
                            committed = init.Value;
                            committedIsSet = true;
                        }
                    }

                    // Forced in the sample phase as well as on demand, because the behavior this
                    // replaces forced its lazy initial value there whether or not anything fired.
                    trans1.Sample(EnsureCommittedIsSet);

                    IListener l = this.Listen(
                        @out.Node,
                        trans1,
                        (trans2, a) =>
                        {
                            EnsureCommittedIsSet();

                            bool emit = !(committed.TryGetValue(out T last) && areEqual(last, a));

                            if (!hasPending)
                            {
                                hasPending = true;
                                trans2.Last(
                                    () =>
                                    {
                                        committed = pending;
                                        hasPending = false;
                                    });
                            }

                            // Assigned on every firing, not just the ones that get through: Collect
                            // fed its state back for suppressed firings too, carrying the unchanged
                            // value forward.
                            pending = emit ? MaybeInternal.Some(a) : committed;

                            if (emit)
                            {
                                @out.Send(trans2, a);
                            }
                        },
                        false);

                    return @out.UnsafeAttachListener(l);
                });

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
                });
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
                });
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
            if (this.attachedListeners == null)
            {
                this.attachedListeners = new List<IListener>();
            }

            this.attachedListeners.Add(cleanup);
            this.trackedListeners.AddListener(cleanup.GetListenerWithWeakReference());
            return this;
        }

        internal void Send(TransactionInternal trans, T a)
        {
            if (this.firings == null)
            {
                this.firings = new List<T>();
                this.clearFirings = this.firings.Clear;
            }

            if (this.firings.Count < 1)
            {
                trans.Last(this.clearFirings);
            }

            this.firings.Add(a);

            foreach (Node<T>.Target target in this.Node.GetListenersCopy())
            {
                // SendEntry rather than a lambda: this runs for every target of every firing,
                // and a closure here costs a display class and a delegate on top of the queue
                // entry that has to be allocated anyway. Carrying the three captured values as
                // fields on the entry collapses that to a single allocation.
                trans.Prioritized(new SendEntry(this, target, a));
            }
        }

        private sealed class SendEntry : TransactionInternal.Entry
        {
            private readonly Stream<T> stream;
            private readonly Node<T>.Target target;
            private readonly T value;

            public SendEntry(Stream<T> stream, Node<T>.Target target, T value)
                : base(target.Node)
            {
                this.stream = stream;
                this.target = target;
                this.value = value;
            }

            public override void Execute(TransactionInternal trans)
            {
                trans.InCallback++;
                try
                {
                    // Don't allow transactions to interfere with Sodium
                    // internals.
                    // Dereference the weak reference
                    if (this.target.Action.TryGetTarget(out Action<TransactionInternal, T> action))
                    {
                        // If it hasn't been garbage collected, call it.
                        if (this.target.IsActivated)
                        {
                            action(trans, this.value);
                        }
                    }
                    else
                    {
                        // If it has been garbage collected, remove it.
                        this.stream.Node.RemoveListener(this.target);
                    }
                }
                finally
                {
                    trans.InCallback--;
                }
            }
        }

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
            // One of these exists per root stream, and plenty of streams are never listened to at
            // all, so both collections wait until something actually needs them.
            private HashSet<IListener> listeners;

            // ReSharper disable once CollectionNeverQueried.Local
            private List<IKeepListenersAlive> childKeepListenersAliveList;

            public void KeepListenerAlive(IListener listener)
            {
                if (this.listeners == null)
                {
                    this.listeners = new HashSet<IListener>();
                }

                this.listeners.Add(listener);
            }

            public void StopKeepingListenerAlive(IListener listener)
            {
                this.listeners?.Remove(listener);
            }

            public void Use(IKeepListenersAlive childKeepListenersAlive)
            {
                if (this.childKeepListenersAliveList == null)
                {
                    this.childKeepListenersAliveList = new List<IKeepListenersAlive>();
                }

                this.childKeepListenersAliveList.Add(childKeepListenersAlive);
            }
        }
    }
}