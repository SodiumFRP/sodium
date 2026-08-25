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
        private readonly List<Action<TransactionInternal>> sendQueue = new List<Action<TransactionInternal>>();
        private List<Action> sampleQueue = new List<Action>();
        private readonly Queue<Action> lastQueue = new Queue<Action>();
        private readonly Queue<Action<TransactionInternal>> postQueue;
        private Dictionary<int, Action<TransactionInternal>> splitQueue;
        private readonly bool hasParentTransaction;
        internal readonly List<Node.Target> TargetsToActivate;
        internal bool ActivatedTargets;

        private static readonly EntryPriorityQueue prioritizedQueue = new EntryPriorityQueue();

        public readonly HashSet<Entry> RerankEntriesSet = new HashSet<Entry>();

        internal TransactionInternal()
            : this(new Queue<Action<TransactionInternal>>(), new Dictionary<int, Action<TransactionInternal>>(), false)
        {
        }

        private TransactionInternal(
            Queue<Action<TransactionInternal>> postQueue,
            Dictionary<int, Action<TransactionInternal>> splitQueue,
            bool hasParentTransaction = true)
        {
            this.postQueue = postQueue;
            this.splitQueue = splitQueue;
            this.hasParentTransaction = hasParentTransaction;
            this.TargetsToActivate = new List<Node.Target>();
        }

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

        internal void Send(Action<TransactionInternal> action) => this.sendQueue.Add(action);

        internal void Prioritized(Node node, Action<TransactionInternal> action) =>
            this.Prioritized(new ActionEntry(node, action));

        internal void Prioritized(Entry e)
        {
            lock (Node.NodeRanksLock)
            {
                prioritizedQueue.Enqueue(e);
            }
        }

        internal void Sample(Action action) => this.sampleQueue.Add(action);

        /// <summary>
        ///     Add an action to run after all prioritized actions.
        /// </summary>
        /// <param name="action">The action to run after all prioritized actions.</param>
        internal void Last(Action action) => this.lastQueue.Enqueue(action);

        /// <summary>
        ///     Add an action to run after all last actions.
        /// </summary>
        /// <param name="action">The action to run after all last actions.</param>
        internal UnitInternal Post(Action<TransactionInternal> action)
        {
            this.postQueue.Enqueue(action);

            return UnitInternal.Value;
        }

        /// <summary>
        ///     Add an action to run after all last actions.
        /// </summary>
        /// <param name="index">The order index in which to run the action.</param>
        /// <param name="action">The action to run after all last actions.</param>
        internal UnitInternal Split(int index, Action<TransactionInternal> action)
        {
            // If an entry exists already, combine the old one with the new one.
            Action<TransactionInternal> @new;
            if (this.splitQueue.TryGetValue(index, out Action<TransactionInternal> existing))
            {
                @new = existing + action;
            }
            else
            {
                @new = action;
            }

            this.splitQueue[index] = @new;

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
            foreach (Entry entry in this.RerankEntriesSet)
            {
                prioritizedQueue.ChangeRank(entry, entry.Node.Rank);
            }

            this.RerankEntriesSet.Clear();
        }

        internal void Close()
        {
            try
            {
                EnsureElevated(this);

                foreach (Node.Target target in this.TargetsToActivate)
                {
                    target.IsActivated = true;
                }

                this.ActivatedTargets = true;

                // ReSharper disable once ForCanBeConvertedToForeach
                for (int i = 0; i < this.sendQueue.Count; i++)
                {
                    this.sendQueue[i](this);
                }

                this.sendQueue.Clear();

                while (!prioritizedQueue.IsEmpty() || this.sampleQueue.Count > 0)
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
                    this.sampleQueue = new List<Action>();
                    foreach (Action s in sq)
                    {
                        s();
                    }
                }

                while (this.lastQueue.Count > 0)
                {
                    this.lastQueue.Dequeue()();
                }

                if (!this.hasParentTransaction)
                {
                    void ExecuteInNewTransaction(Action<TransactionInternal> action, bool runStartHooks)
                    {
                        try
                        {
                            TransactionInternal transaction = new TransactionInternal(this.postQueue, this.splitQueue);

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

                    while (this.postQueue.Count > 0 || this.splitQueue.Count > 0)
                    {
                        while (this.postQueue.Count > 0)
                        {
                            ExecuteInNewTransaction(this.postQueue.Dequeue(), true);
                        }

                        Dictionary<int, Action<TransactionInternal>> sq = this.splitQueue;
                        this.splitQueue = new Dictionary<int, Action<TransactionInternal>>();

                        List<int> splitIndexes = new List<int>(sq.Keys);
                        splitIndexes.Sort();
                        foreach (int n in splitIndexes)
                        {
                            ExecuteInNewTransaction(sq[n], false);
                        }
                    }
                }
            }
            catch
            {
                this.sendQueue.Clear();
                
                while (!prioritizedQueue.IsEmpty())
                {
                    Entry e = prioritizedQueue.Dequeue();
                    e.IsRemoved = true;
                    e.Dispose();
                }
                
                this.sampleQueue = new List<Action>();
                
                this.lastQueue.Clear();
                
                this.postQueue.Clear();
                
                this.splitQueue = new Dictionary<int, Action<TransactionInternal>>();
                
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
                this.nodeEntryIndex = node.Entries.Count;
                node.Entries.Add(this);
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
                // RerankEntriesSet, a HashSet, so nothing depends on the order - and a wide
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