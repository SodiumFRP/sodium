using System;
using System.Collections.Generic;
using System.Threading;
using Priority_Queue;

namespace Sodium
{
    /// <summary>
    ///     A class for managing transactions.
    /// </summary>
    public sealed class Transaction
    {
        // Coarse-grained lock that's held during the whole transaction.
        private static readonly object TransactionLock = new object();

        private static Transaction currentTransaction;
        private static readonly ThreadLocal<Transaction> localTransaction = new ThreadLocal<Transaction>();
        internal static int InCallback;
        private static readonly List<Action> OnStartHooks = new List<Action>();
        private static bool runningOnStartHooks;
        private readonly HashSet<Entry> entries = new HashSet<Entry>();
        private readonly List<Action> lastQueue = new List<Action>();
        private readonly Dictionary<int, Action<Transaction>> postQueue = new Dictionary<int, Action<Transaction>>();

        private readonly SimplePriorityQueue<Entry> prioritizedQueue = new SimplePriorityQueue<Entry>();

        // True if we need to re-generate the priority queue.
        private bool toRegen;

        internal bool ReachedClose;

        /// <summary>
        ///     Return whether or not there is a current transaction.
        /// </summary>
        /// <returns><code>true</code> if there is a current transaction, <code>false</code> otherwise.</returns>
        internal static bool HasCurrentTransaction()
        {
            if (localTransaction.Value != null)
            {
                return true;
            }

            lock (TransactionLock)
            {
                return currentTransaction != null;
            }
        }

        /// <summary>
        ///     Execute the specified action inside a single transaction.
        /// </summary>
        /// <param name="action">The action to execute.</param>
        /// <remarks>
        ///     In most cases this is not needed, because all primitives will create their own transaction automatically.
        ///     It is useful for running multiple reactive operations atomically.
        /// </remarks>
        public static void RunVoid(Action action)
        {
            Apply(_ =>
            {
                action();
                return Unit.Value;
            });
        }

        /// <summary>
        ///     Execute the specified function inside a single transaction.
        /// </summary>
        /// <typeparam name="T">The type of the value returned.</typeparam>
        /// <param name="f">The function to execute.</param>
        /// <returns>The return value of <paramref name="f" />.</returns>
        /// <remarks>
        ///     In most cases this is not needed, because all primitives will create their own transaction automatically.
        ///     It is useful for running multiple reactive operations atomically.
        /// </remarks>
        public static T Run<T>(Func<T> f)
        {
            return Apply(_ => f());
        }

        /// <summary>
        ///     Execute the specified function inside a single transaction.
        ///     The function should only be used to construct FRP logic and may not close over any other FRP logic.
        ///     This transaction will not block other transactions from running.
        /// </summary>
        /// <typeparam name="T">The type of the value returned.</typeparam>
        /// <param name="f">The function to execute.</param>
        /// <returns>The return value of <paramref name="f" />.</returns>
        /// <remarks>
        ///     This method is most useful for creatung FRP logic which must be created within a Transaction (such as in a loop),
        ///     but which should not block other transactions from running.  A use case for this is if the construction of FRP logic takes
        ///     a significant amount of time, is being done asynchronously, and may be cancelled by another stream event.
        /// </remarks>
        public static T RunConstruct<T>(Func<T> f)
        {
            return Apply(localTransaction.Value, true, _ => f());
        }

        private static T Apply<T>(Transaction transaction, bool usingLocal, Func<Transaction, T> code)
        {
            Transaction newTransaction = transaction;
            try
            {
                if (newTransaction == null)
                {
                    newTransaction = usingLocal ? new Transaction() : Start();
                    SetCurrentTransaction(newTransaction, usingLocal);
                }

                return code(newTransaction);
            }
            finally
            {
                try
                {
                    if (transaction == null)
                    {
                        newTransaction?.Close(usingLocal);
                    }
                }
                finally
                {
                    if (transaction == null)
                    {
                        SetCurrentTransaction(null, usingLocal);
                    }
                }
            }
        }

        internal static T Apply<T>(Func<Transaction, T> code)
        {
            Transaction lt = localTransaction.Value;
            if (lt != null)
            {
                return Apply(lt, true, code);
            }

            lock (TransactionLock)
            {
                return Apply(currentTransaction, false, code);
            }
        }

        /// <summary>
        ///     Add an action that will be executed whenever a transaction is started.
        /// </summary>
        /// <param name="action"></param>
        /// <remarks>
        ///     The action may start transactions itself, which will not cause the hooks to execute recursively.
        ///     The main use case of this is for the implementation of a time/alarm system.
        /// </remarks>
        public static void OnStart(Action action)
        {
            lock (TransactionLock)
            {
                OnStartHooks.Add(action);
            }
        }

        private static Transaction Start()
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

            return new Transaction();
        }

        internal void Prioritized(Node rank, Action<Transaction> action)
        {
            Entry e = new Entry(rank, action);
            this.prioritizedQueue.Enqueue(e, rank.Rank);
            this.entries.Add(e);
        }

        /// <summary>
        ///     Add an action to run after all prioritized actions.
        /// </summary>
        /// <param name="action">The action to run after all prioritized actions.</param>
        internal void Last(Action action)
        {
            this.lastQueue.Add(action);
        }

        /// <summary>
        ///     Add an action to run after all last actions.
        /// </summary>
        /// <param name="index">The order index in which to run the action.</param>
        /// <param name="action">The action to run after all last actions.</param>
        internal Unit Post(int index, Action<Transaction> action)
        {
            // If an entry exists already, combine the old one with the new one.
            Action<Transaction> @new;
            Action<Transaction> existing;
            if (this.postQueue.TryGetValue(index, out existing))
            {
                @new = trans =>
                {
                    existing(trans);
                    action(trans);
                };
            }
            else
            {
                @new = action;
            }

            this.postQueue[index] = @new;

            return Unit.Value;
        }

        /// <summary>
        ///     Execute an action after the current transaction is closed
        ///     or immediately if there is no current transaction.
        /// </summary>
        /// <param name="action">
        ///     The action to run after the current transaction is closed
        ///     or immediately if there is no current transaction.
        /// </param>
        public static void Post(Action action)
        {
            // -1 will mean it runs before anything split/deferred, and will run
            // outside a transaction context.
            Apply(trans => trans.Post(-1, _ => action()));
        }

        internal void SetNeedsRegenerating()
        {
            this.toRegen = true;
        }

        // If the priority queue has entries in it when we modify any of the nodes'
        // ranks, then we need to re-generate it to make sure it's up-to-date.
        private void CheckRegen()
        {
            if (this.toRegen)
            {
                this.toRegen = false;
                this.prioritizedQueue.Clear();
                foreach (Entry e in this.entries)
                {
                    this.prioritizedQueue.Enqueue(e, e.Rank.Rank);
                }
            }
        }

        internal void Close(bool usingLocal)
        {
            ReachedClose = true;

            foreach (Entry entry in this.entries)
            {
                if (entry.Rank.FixRank())
                {
                    this.SetNeedsRegenerating();
                }
            }
            this.CheckRegen();

            while (true)
            {
                if (this.prioritizedQueue.Count < 1)
                {
                    break;
                }

                Entry e = this.prioritizedQueue.Dequeue();
                this.entries.Remove(e);
                e.Action(this);

                this.CheckRegen();
            }

            foreach (Action action in this.lastQueue)
            {
                action();
            }
            this.lastQueue.Clear();

            foreach (KeyValuePair<int, Action<Transaction>> pair in this.postQueue)
            {
                try
                {
                    if (pair.Key < 0)
                    {
                        SetCurrentTransaction(null, usingLocal);
                        pair.Value(null);
                    }
                    else
                    {
                        Transaction transaction = new Transaction();
                        SetCurrentTransaction(transaction, usingLocal);
                        try
                        {
                            pair.Value(transaction);
                        }
                        finally
                        {
                            transaction.Close(usingLocal);
                        }
                    }
                }
                finally
                {
                    SetCurrentTransaction(this, usingLocal);
                }
            }
            this.postQueue.Clear();
        }

        private static void SetCurrentTransaction(Transaction transaction, bool usingLocal)
        {
            if (usingLocal)
            {
                localTransaction.Value = transaction;
            }
            else
            {
                currentTransaction = transaction;
            }
        }

        private class Entry : IComparable<Entry>
        {
            private static long nextSeq;

            public readonly Node Rank;
            public readonly Action<Transaction> Action;
            private readonly long seq;

            public Entry(Node rank, Action<Transaction> action)
            {
                this.Rank = rank;
                this.Action = action;
                this.seq = nextSeq++;
            }

            public int CompareTo(Entry other)
            {
                int answer = this.Rank.CompareTo(other.Rank);
                return answer != 0 ? answer : this.seq.CompareTo(other.seq);
            }
        }
    }
}