using System;
using System.Collections.Generic;
using System.Runtime.ExceptionServices;
using System.Threading;

namespace Sodium.Frp
{
    /// <summary>
    ///     A class for managing transactions.
    /// </summary>
    internal sealed class TransactionInternal
    {
        // [ThreadStatic] rather than ThreadLocal<T>: these are read on essentially every public
        // entry point, and a thread-static field is a direct TLS access where ThreadLocal<T>.Value
        // goes through a generic slot table. Nothing here uses ThreadLocal's extra surface
        // (Values, IsValueCreated, value factories, disposal), so the two are interchangeable.
        [ThreadStatic]
        private static TransactionInternal localTransaction;

        [ThreadStatic]
        private static bool runningOnStartHooks;

        // Coarse-grained lock that's held during the whole transaction.
        //
        // Serializing every transaction process-wide is a deliberate guarantee of the library, not an
        // implementation artifact: it is what makes a transaction atomic across threads and keeps update
        // ordering deterministic, which is why callers need no synchronization of their own. Narrowing or
        // sharding this lock would change the library's threading semantics, however tempting it looks as a
        // contention fix. The guarantee and its consequences are documented in the remarks on the public
        // Sodium.Frp.Transaction class.
        private static readonly object TransactionLock = new object();

        private bool isElevated;
        private bool obtainedLock;
        internal int InCallback;
        private static readonly List<Action> OnStartHooks = new List<Action>();
        // All of these are allocated on first use rather than in the constructor. A transaction
        // is created for every send that is not already inside one, and the great majority of
        // them touch only a couple of these - eagerly allocating all seven, the dictionary and
        // the set in particular, was the majority of what an empty transaction cost.
        private List<Action<TransactionInternal>> sendQueue;
        private List<Action> sampleQueue;
        private Queue<Action> lastQueue;
        private List<Node.Target> targetsToActivate;
        private HashSet<Entry> rerankEntriesSet;

        // The post and split queues belong to the root transaction and are shared with the child
        // transactions it spawns while closing, so that work deferred from inside a deferred
        // action lands in the queue the root is still draining. Children reach them through
        // deferredOwner rather than holding their own.
        private readonly TransactionInternal deferredOwner;
        private Queue<Action<TransactionInternal>> postQueue;
        private Dictionary<int, Action<TransactionInternal>> splitQueue;

        private readonly bool hasParentTransaction;
        internal bool ActivatedTargets;

        private static readonly EntryPriorityQueue prioritizedQueue = new EntryPriorityQueue();

        internal TransactionInternal()
        {
        }

        private TransactionInternal(TransactionInternal deferredOwner)
        {
            this.deferredOwner = deferredOwner;
            this.hasParentTransaction = true;
        }

        private TransactionInternal DeferredOwner => this.deferredOwner ?? this;

        internal static bool IsActiveImpl() => HasCurrentTransaction();

        /// <summary>
        ///     Return whether there is a current transaction.
        /// </summary>
        /// <returns><code>true</code> if there is a current transaction, <code>false</code> otherwise.</returns>
        internal static bool HasCurrentTransaction() => localTransaction != null;

        /// <summary>
        ///     Return the current transaction or <code>null</code>.
        /// </summary>
        /// <returns>The current transaction or <code>null</code>.</returns>
        internal static TransactionInternal GetCurrentTransaction() => localTransaction;

        internal static T RunImpl<T>(Func<T> f) => Apply((_, __) => f());

        internal static T Apply<T>(Func<TransactionInternal, bool, T> code)
        {
            TransactionInternal transaction = localTransaction;

            T returnValue = default;
            Exception exception = null;
            TransactionInternal newTransaction = transaction;
            try
            {
                bool createdNewTransaction = newTransaction == null;
                if (newTransaction == null)
                {
                    newTransaction = new TransactionInternal();

                    localTransaction = newTransaction;
                }

                EnsureElevated(newTransaction);

                returnValue = code(newTransaction, createdNewTransaction);
            }
            catch (Exception e)
            {
                exception = e;
            }

            try
            {
                try
                {
                    if (transaction == null)
                    {
                        newTransaction?.Close();
                    }
                }
                catch (Exception e)
                {
                    if (exception == null)
                    {
                        throw;
                    }

                    throw new AggregateException(exception, e);
                }

                if (exception != null)
                {
                    ExceptionDispatchInfo.Capture(exception).Throw();
                }

                return returnValue;
            }
            finally
            {
                if (transaction == null)
                {
                    if (newTransaction != null && newTransaction.obtainedLock)
                    {
                        Monitor.Exit(TransactionLock);
                    }

                    localTransaction = null;
                }
            }
        }

        private static void EnsureElevated(TransactionInternal transaction)
        {
            if (transaction != null && !transaction.isElevated)
            {
                transaction.isElevated = true;

                if (!runningOnStartHooks)
                {
                    if (!transaction.hasParentTransaction)
                    {
                        Monitor.Enter(TransactionLock);
                        transaction.obtainedLock = true;
                    }

                    RunStartHooks(transaction);
                }
            }
        }

        internal static void OnStartImpl(Action action)
        {
            lock (TransactionLock)
            {
                OnStartHooks.Add(action);
            }
        }

        private static void RunStartHooks(TransactionInternal transaction)
        {
            if (OnStartHooks.Count > 0)
            {
                try
                {
                    localTransaction = null;
                    runningOnStartHooks = true;

                    foreach (Action action in OnStartHooks)
                    {
                        action();
                    }
                }
                finally
                {
                    localTransaction = transaction;
                    runningOnStartHooks = false;
                }
            }
        }

        internal void Send(Action<TransactionInternal> action) =>
            (this.sendQueue ?? (this.sendQueue = new List<Action<TransactionInternal>>())).Add(action);

        internal void AddTargetToActivate(Node.Target target) =>
            (this.targetsToActivate ?? (this.targetsToActivate = new List<Node.Target>())).Add(target);

        internal void AddRerankEntry(Entry entry) =>
            (this.rerankEntriesSet ?? (this.rerankEntriesSet = new HashSet<Entry>())).Add(entry);

        internal void Prioritized(Node node, Action<TransactionInternal> action) =>
            this.Prioritized(new ActionEntry(node, action));

        internal void Prioritized(Entry e)
        {
            lock (Node.NodeRanksLock)
            {
                prioritizedQueue.Enqueue(e);
            }
        }

        internal void Sample(Action action) =>
            (this.sampleQueue ?? (this.sampleQueue = new List<Action>())).Add(action);

        /// <summary>
        ///     Add an action to run after all prioritized actions.
        /// </summary>
        /// <param name="action">The action to run after all prioritized actions.</param>
        internal void Last(Action action) =>
            (this.lastQueue ?? (this.lastQueue = new Queue<Action>())).Enqueue(action);

        /// <summary>
        ///     Add an action to run after all last actions.
        /// </summary>
        /// <param name="action">The action to run after all last actions.</param>
        internal UnitInternal Post(Action<TransactionInternal> action)
        {
            TransactionInternal owner = this.DeferredOwner;
            (owner.postQueue ?? (owner.postQueue = new Queue<Action<TransactionInternal>>())).Enqueue(action);

            return UnitInternal.Value;
        }

        /// <summary>
        ///     Add an action to run after all last actions.
        /// </summary>
        /// <param name="index">The order index in which to run the action.</param>
        /// <param name="action">The action to run after all last actions.</param>
        internal UnitInternal Split(int index, Action<TransactionInternal> action)
        {
            TransactionInternal owner = this.DeferredOwner;
            Dictionary<int, Action<TransactionInternal>> queue =
                owner.splitQueue ?? (owner.splitQueue = new Dictionary<int, Action<TransactionInternal>>());

            // If an entry exists already, combine the old one with the new one.
            Action<TransactionInternal> @new;
            if (queue.TryGetValue(index, out Action<TransactionInternal> existing))
            {
                @new = existing + action;
            }
            else
            {
                @new = action;
            }

            queue[index] = @new;

            return UnitInternal.Value;
        }

        internal static void PostImpl(Action action)
        {
            // -1 will mean it runs before anything split/deferred, and will run
            // outside a transaction context.
            Apply(
                (trans, createdNewTransaction) =>
                {
                    if (createdNewTransaction)
                    {
                        action();
                    }
                    else
                    {
                        trans.Post(_ => action());
                    }

                    return UnitInternal.Value;
                });
        }

        // If the priority queue has entries in it when we modify any of the nodes'
        // ranks, then we need to re-generate it to make sure it's up-to-date.
        private void CheckRegen()
        {
            if (this.rerankEntriesSet == null)
            {
                return;
            }

            foreach (Entry entry in this.rerankEntriesSet)
            {
                prioritizedQueue.ChangeRank(entry, entry.Node.Rank);
            }

            this.rerankEntriesSet.Clear();
        }

        internal void Close()
        {
            try
            {
                EnsureElevated(this);

                if (this.targetsToActivate != null)
                {
                    foreach (Node.Target target in this.targetsToActivate)
                    {
                        target.IsActivated = true;
                    }
                }

                this.ActivatedTargets = true;

                if (this.sendQueue != null)
                {
                    // ReSharper disable once ForCanBeConvertedToForeach
                    for (int i = 0; i < this.sendQueue.Count; i++)
                    {
                        this.sendQueue[i](this);
                    }

                    this.sendQueue.Clear();
                }

                while (!prioritizedQueue.IsEmpty() || this.sampleQueue?.Count > 0)
                {
                    while (!prioritizedQueue.IsEmpty())
                    {
                        this.CheckRegen();

                        Entry e = prioritizedQueue.Dequeue();
                        e.IsRemoved = true;
                        e.Execute(this);
                        e.Dispose();
                    }

                    List<Action> sq = this.sampleQueue;
                    this.sampleQueue = null;
                    if (sq != null)
                    {
                        foreach (Action s in sq)
                        {
                            s();
                        }
                    }
                }

                while (this.lastQueue?.Count > 0)
                {
                    this.lastQueue.Dequeue()();
                }

                if (!this.hasParentTransaction)
                {
                    void ExecuteInNewTransaction(Action<TransactionInternal> action, bool runStartHooks)
                    {
                        try
                        {
                            // The child defers back into this transaction's queues, so a Post or
                            // Split made from inside a deferred action joins the drain already
                            // in progress here rather than being stranded on the child.
                            TransactionInternal transaction = new TransactionInternal(this);

                            if (!runStartHooks)
                            {
                                // this will ensure we don't run start hooks
                                transaction.isElevated = true;
                            }

                            localTransaction = transaction;
                            try
                            {
                                action(transaction);
                            }
                            finally
                            {
                                transaction.Close();
                            }
                        }
                        finally
                        {
                            localTransaction = this;
                        }
                    }

                    while (this.postQueue?.Count > 0 || this.splitQueue?.Count > 0)
                    {
                        while (this.postQueue?.Count > 0)
                        {
                            ExecuteInNewTransaction(this.postQueue.Dequeue(), true);
                        }

                        Dictionary<int, Action<TransactionInternal>> sq = this.splitQueue;
                        this.splitQueue = null;

                        if (sq != null)
                        {
                            List<int> splitIndexes = new List<int>(sq.Keys);
                            splitIndexes.Sort();
                            foreach (int n in splitIndexes)
                            {
                                ExecuteInNewTransaction(sq[n], false);
                            }
                        }
                    }
                }
            }
            catch
            {
                this.sendQueue?.Clear();

                while (!prioritizedQueue.IsEmpty())
                {
                    Entry e = prioritizedQueue.Dequeue();
                    e.IsRemoved = true;
                    e.Dispose();
                }

                this.sampleQueue = null;

                this.lastQueue?.Clear();

                this.postQueue?.Clear();

                this.splitQueue = null;

                throw;
            }
        }

        internal abstract class Entry : IDisposable
        {
            public readonly Node Node;
            public bool InPq;
            public int PqRank;
            public Entry PqNext;
            public Entry PqPrev;
            public bool IsRemoved;

            // Where this entry sits in Node.Entries, so removal needs neither a search nor a
            // shift. -1 means "not in the list", which also makes a second Dispose a no-op
            // rather than removing whatever entry now occupies the old position.
            private int nodeEntryIndex;

            protected Entry(Node node)
            {
                this.Node = node;
                this.PqRank = node.Rank;
                this.nodeEntryIndex = node.AddEntry(this);
            }

            // Subclasses carry whatever state the queued work needs as fields, so a caller on a
            // hot path can avoid allocating a closure and a delegate on top of the entry itself.
            public abstract void Execute(TransactionInternal trans);

            public void Dispose()
            {
                int index = this.nodeEntryIndex;
                if (index < 0)
                {
                    return;
                }

                this.nodeEntryIndex = -1;

                // Swap the last entry into this slot rather than shifting everything after it
                // down. Node.Entries is only ever walked to collect entries into
                // rerankEntriesSet, a HashSet, so nothing depends on the order - and a wide
                // fan-in (Cell.Lift over N cells links all N to one node) makes the repeated
                // RemoveAt(0) that this replaces quadratic in the number of entries.
                List<Entry> entries = this.Node.Entries;
                int last = entries.Count - 1;
                if (index != last)
                {
                    Entry moved = entries[last];
                    entries[index] = moved;
                    moved.nodeEntryIndex = index;
                }

                entries.RemoveAt(last);
            }
        }

        // The general-purpose entry, for the call sites that run once per construction or once
        // per transaction and so have nothing to gain from avoiding the delegate.
        internal sealed class ActionEntry : Entry
        {
            private readonly Action<TransactionInternal> action;

            public ActionEntry(Node node, Action<TransactionInternal> action)
                : base(node) => this.action = action;

            public override void Execute(TransactionInternal trans) => this.action(trans);
        }
    }
}