using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.ExceptionServices;
using System.Threading;
using Priority_Queue;

namespace Sodium.Frp
{
    /// <summary>
    ///     A class for managing transactions.
    /// </summary>
    internal sealed class TransactionInternal
    {
        private static readonly ThreadLocal<TransactionInternal> LocalTransaction = new ThreadLocal<TransactionInternal>();

        // Coarse-grained lock that's held during the whole transaction.
        private static readonly object TransactionLock = new object();

        private bool isElevated;
        internal static int InCallback;
        private static readonly List<Action> OnStartHooks = new List<Action>();
        private static bool runningOnStartHooks;
        private List<Entry> entries = new List<Entry>();
        private readonly List<Action<TransactionInternal>> sendQueue = new List<Action<TransactionInternal>>();
        private List<Action> sampleQueue = new List<Action>();
        private readonly Queue<Action> lastQueue = new Queue<Action>();
        private readonly Queue<Action<TransactionInternal>> postQueue;
        private Dictionary<int, Action<TransactionInternal>> splitQueue;
        private readonly bool hasParentTransaction;
        internal readonly List<Node.Target> TargetsToActivate;
        internal bool ActivatedTargets;

        private readonly SimplePriorityQueue<Entry, long> prioritizedQueue = new SimplePriorityQueue<Entry, long>();

        // True if we need to re-generate the priority queue.
        private bool toRegen;

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
        ///     Return whether or not there is a current transaction.
        /// </summary>
        /// <returns><code>true</code> if there is a current transaction, <code>false</code> otherwise.</returns>
        internal static bool HasCurrentTransaction() => LocalTransaction.Value != null;

        /// <summary>
        ///     Return the current transaction or <code>null</code>.
        /// </summary>
        /// <returns>The current transaction or <code>null</code>.</returns>
        internal static TransactionInternal GetCurrentTransaction() => LocalTransaction.Value;

        internal static T RunImpl<T>(Func<T> f) => Apply((_, __) => f(), false);

        internal static T Apply<T>(Func<TransactionInternal, bool, T> code, bool ensureElevated)
        {
            TransactionInternal transaction = LocalTransaction.Value;

            T returnValue = default(T);
            Exception exception = null;
            TransactionInternal newTransaction = transaction;
            try
            {
                bool createdNewTransaction = newTransaction == null;
                if (newTransaction == null)
                {
                    newTransaction = new TransactionInternal();

                    LocalTransaction.Value = newTransaction;
                }

                if (ensureElevated)
                {
                    EnsureElevated(newTransaction);
                }

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
                    if (newTransaction != null && newTransaction.isElevated && !newTransaction.hasParentTransaction)
                    {
                        Monitor.Exit(TransactionLock);
                    }

                    LocalTransaction.Value = null;
                }
            }
        }

        private static void EnsureElevated(TransactionInternal transaction)
        {
            if (transaction != null && !transaction.isElevated)
            {
                transaction.isElevated = true;

                if (!transaction.hasParentTransaction)
                {
                    Monitor.Enter(TransactionLock);
                }

                RunStartHooks();
            }
        }

        internal static void OnStartImpl(Action action)
        {
            lock (TransactionLock)
            {
                OnStartHooks.Add(action);
            }
        }

        private static void RunStartHooks()
        {
            if (!runningOnStartHooks)
            {
                runningOnStartHooks = true;
                try
                {
                    foreach (Action action in OnStartHooks)
                    {
                        action();
                    }
                }
                finally
                {
                    runningOnStartHooks = false;
                }
            }
        }

        internal void Send(Action<TransactionInternal> action) => this.sendQueue.Add(action);

        internal void Prioritized(Node node, Action<TransactionInternal> action)
        {
            Entry e = new Entry(node, action);
            lock (Node.NodeRanksLock)
            {
                this.prioritizedQueue.Enqueue(e, node.Rank);
            }

            this.entries.Add(e);
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
                },
                false);
        }

        internal void SetNeedsRegenerating() => this.toRegen = true;

        // If the priority queue has entries in it when we modify any of the nodes'
        // ranks, then we need to re-generate it to make sure it's up-to-date.
        private void CheckRegen()
        {
            if (this.toRegen)
            {
                this.toRegen = false;
                this.prioritizedQueue.Clear();
                lock (Node.NodeRanksLock)
                {
                    List<Entry> newEntries = new List<Entry>(this.entries.Count);
                    foreach (Entry e in this.entries)
                    {
                        if (!e.IsRemoved)
                        {
                            newEntries.Add(e);
                            this.prioritizedQueue.Enqueue(e, e.Node.Rank);
                        }
                    }

                    this.entries = newEntries;
                }
            }
        }

        internal void Close()
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

            while (this.prioritizedQueue.Count > 0 || this.sampleQueue.Count > 0)
            {
                while (this.prioritizedQueue.Count > 0)
                {
                    this.CheckRegen();

                    Entry e = this.prioritizedQueue.Dequeue();
                    e.IsRemoved = true;
                    e.Action(this);
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

                        LocalTransaction.Value = transaction;
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
                        LocalTransaction.Value = this;
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
                    foreach (int n in sq.Keys.OrderBy(n => n))
                    {
                        ExecuteInNewTransaction(sq[n], false);
                    }
                }
            }
        }

        private class Entry
        {
            public readonly Node Node;
            public readonly Action<TransactionInternal> Action;
            public bool IsRemoved;

            public Entry(Node node, Action<TransactionInternal> action)
            {
                this.Node = node;
                this.Action = action;
            }
        }
    }
}