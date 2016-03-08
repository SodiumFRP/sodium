using System;
using System.Collections.Generic;
using System.Linq;
using Priority_Queue;

namespace Sodium
{
    /// <summary>
    ///     A class for managing transactions.
    /// </summary>
    public sealed class Transaction
    {
        // Coarse-grained lock that's held during the whole transaction.
        internal static readonly object TransactionLock = new object();
        // Fine-grained lock that protects listeners and nodes.
        internal static readonly object ListenersLock = new object();

        private static Transaction currentTransaction;
        internal static int InCallback;
        private static readonly List<Action> OnStartHooks = new List<Action>();
        private static bool runningOnStartHooks;
        private readonly HashSet<Entry> entries = new HashSet<Entry>();
        private readonly List<Action> lastQueue = new List<Action>();
        private readonly Dictionary<int, Action<Transaction>> postQueue = new Dictionary<int, Action<Transaction>>();

        private readonly SimplePriorityQueue<Entry> prioritizedQueue = new SimplePriorityQueue<Entry>();

        // True if we need to re-generate the priority queue.
        private bool toRegen;

        /// <summary>
        ///     Return the current transaction as an option type.
        /// </summary>
        /// <returns>The current transaction as an option type.</returns>
        public static IMaybe<Transaction> GetCurrentTransaction()
        {
            lock (TransactionLock)
            {
                return currentTransaction == null ? Maybe.Nothing<Transaction>() : Maybe.Just(currentTransaction);
            }
        }

        /// <summary>
        ///     Return whether or not there is a current transaction.
        /// </summary>
        /// <returns><code>true</code> if there is a current transaction, <code>false</code> otherwise.</returns>
        public static bool HasCurrentTransaction()
        {
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
            lock (TransactionLock)
            {
                // If we are already inside a transaction (which must be on the same
                // thread otherwise we wouldn't have acquired transactionLock), then
                // keep using that same transaction.
                Transaction transWas = currentTransaction;
                try
                {
                    StartIfNecessary();
                    action();
                }
                finally
                {
                    try
                    {
                        if (transWas == null)
                        {
                            currentTransaction?.Close();
                        }
                    }
                    finally
                    {
                        currentTransaction = transWas;
                    }
                }
            }
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
            lock (TransactionLock)
            {
                // If we are already inside a transaction (which must be on the same
                // thread otherwise we wouldn't have acquired transactionLock), then
                // keep using that same transaction.
                Transaction transWas = currentTransaction;
                try
                {
                    StartIfNecessary();
                    return f();
                }
                finally
                {
                    if (transWas == null)
                    {
                        currentTransaction?.Close();
                    }

                    currentTransaction = transWas;
                }
            }
        }

        internal static void Run(Action<Transaction> code)
        {
            lock (TransactionLock)
            {
                // If we are already inside a transaction (which must be on the same
                // thread otherwise we wouldn't have acquired transactionLock), then
                // keep using that same transaction.
                Transaction transWas = currentTransaction;
                try
                {
                    StartIfNecessary();
                    code(currentTransaction);
                }
                finally
                {
                    try
                    {
                        if (transWas == null)
                        {
                            currentTransaction?.Close();
                        }
                    }
                    finally
                    {
                        currentTransaction = transWas;
                    }
                }
            }
        }

        internal static T Apply<T>(Func<Transaction, T> code)
        {
            lock (TransactionLock)
            {
                // If we are already inside a transaction (which must be on the same
                // thread otherwise we wouldn't have acquired transactionLock), then
                // keep using that same transaction.
                Transaction transWas = currentTransaction;
                try
                {
                    StartIfNecessary();
                    return code(currentTransaction);
                }
                finally
                {
                    if (transWas == null)
                    {
                        currentTransaction?.Close();
                    }

                    currentTransaction = transWas;
                }
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

        private static void StartIfNecessary()
        {
            if (currentTransaction == null)
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

                currentTransaction = new Transaction();
            }
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
        internal void Post(int index, Action<Transaction> action)
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
            Run(trans => trans.Post(-1, _ => action()));
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

        internal void Close()
        {
            while (true)
            {
                this.CheckRegen();

                if (!this.prioritizedQueue.Any())
                {
                    break;
                }

                Entry e = this.prioritizedQueue.Dequeue();
                this.entries.Remove(e);
                e.Action(this);
            }

            foreach (Action action in this.lastQueue)
            {
                action();
            }
            this.lastQueue.Clear();

            foreach (KeyValuePair<int, Action<Transaction>> pair in this.postQueue)
            {
                Transaction parent = currentTransaction;
                try
                {
                    if (pair.Key < 0)
                    {
                        currentTransaction = null;
                        pair.Value(null);
                    }
                    else
                    {
                        Transaction transaction = new Transaction();
                        currentTransaction = transaction;
                        try
                        {
                            pair.Value(transaction);
                        }
                        finally
                        {
                            transaction.Close();
                        }
                    }
                }
                finally
                {
                    currentTransaction = parent;
                }
            }
            this.postQueue.Clear();
        }

        private class Entry : IComparable<Entry>
        {
            private static long nextSeq;

            public readonly Action<Transaction> Action;
            public readonly Node Rank;
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
                if (answer == 0)
                {
                    // Same rank: preserve chronological sequence.
                    if (this.seq < other.seq)
                    {
                        answer = -1;
                    }
                    else if (this.seq > other.seq)
                    {
                        answer = 1;
                    }
                }
                return answer;
            }
        }
    }
}