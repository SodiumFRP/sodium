using System;
using System.Collections.Generic;
using System.Runtime.ExceptionServices;
using System.Threading;
using Priority_Queue;

namespace Sodium
{
    /// <summary>
    ///     A class for managing transactions.
    /// </summary>
    public sealed class Transaction
    {
        private static readonly ThreadLocal<Transaction> LocalTransaction = new ThreadLocal<Transaction>();

        internal readonly bool IsConstructing;

        // Coarse-grained lock that's held during the whole transaction.
        private static readonly object TransactionLock = new object();

        private static Transaction currentTransaction;
        internal static int InCallback;
        private static readonly List<Action> OnStartHooks = new List<Action>();
        private static bool runningOnStartHooks;
        private readonly HashSet<Entry> entries = new HashSet<Entry>();
        private readonly List<Action<Transaction>> sendQueue = new List<Action<Transaction>>();
        private readonly List<Action> lastQueue = new List<Action>();
        private readonly Dictionary<int, Action<Transaction>> postQueue = new Dictionary<int, Action<Transaction>>();
        internal readonly List<Node.Target> TargetsToActivate;

        private readonly SimplePriorityQueue<Entry, long> prioritizedQueue = new SimplePriorityQueue<Entry, long>();

        // True if we need to re-generate the priority queue.
        private bool toRegen;

        internal bool ReachedClose;

        internal Transaction(bool isConstructing)
        {
            this.IsConstructing = isConstructing;
            if (isConstructing)
            {
                this.TargetsToActivate = new List<Node.Target>();
            }
        }

        /// <summary>
        ///     Return whether or not there is a current transaction.
        /// </summary>
        /// <returns><code>true</code> if there is a current transaction, <code>false</code> otherwise.</returns>
        internal static bool HasCurrentTransaction()
        {
            if (Monitor.TryEnter(TransactionLock))
            {
                try
                {
                    if (currentTransaction != null)
                    {
                        return true;
                    }
                }
                finally
                {
                    Monitor.Exit(TransactionLock);
                }
            }

            return LocalTransaction.Value != null;
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
            Apply(
                _ =>
                {
                    action();
                    return Unit.Value;
                },
                false);
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
            return Apply(_ => f(), false);
        }

        /// <summary>
        ///     Execute the specified action inside a single transaction.
        ///     The action should only be used to construct FRP logic and may not close over any other FRP logic.
        ///     This transaction will not block other transactions from running until it reaches the end of the transaction.
        /// </summary>
        /// <param name="action">The action to execute.</param>
        /// <remarks>
        ///     This method is most useful for creating FRP logic which must be created within a Transaction (such as in a loop),
        ///     but which should not block other transactions from running.  A use case for this is if the construction of FRP
        ///     logic takes
        ///     a significant amount of time, is being done asynchronously, and may be canceled by another stream event.
        /// </remarks>
        public static void RunConstructVoid(Action action)
        {
            Apply(
                _ =>
                {
                    action();
                    return Unit.Value;
                },
                true);
        }

        /// <summary>
        ///     Execute the specified function inside a single transaction.
        ///     The function should only be used to construct FRP logic and may not close over any other FRP logic.
        ///     This transaction will not block other transactions from running until it reaches the end of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the value returned.</typeparam>
        /// <param name="f">The function to execute.</param>
        /// <returns>The return value of <paramref name="f" />.</returns>
        /// <remarks>
        ///     This method is most useful for creating FRP logic which must be created within a Transaction (such as in a loop),
        ///     but which should not block other transactions from running.  A use case for this is if the construction of FRP
        ///     logic takes
        ///     a significant amount of time, is being done asynchronously, and may be canceled by another stream event.
        /// </remarks>
        public static T RunConstruct<T>(Func<T> f)
        {
            return Apply(_ => f(), true);
        }

        private static T Apply<T>(Transaction transaction, bool createLocal, Func<Transaction, T> code)
        {
            T returnValue = default(T);
            Exception exception = null;
            Transaction newTransaction = transaction;
            try
            {
                if (newTransaction == null)
                {
                    newTransaction = createLocal ? new Transaction(true) : Start();
                    SetCurrentTransaction(newTransaction, createLocal);
                }

                returnValue = code(newTransaction);
            }
            catch (Exception e)
            {
                exception = e;
            }

            try
            {
                try
                {
                    if (transaction == null && newTransaction != null)
                    {
                        if (createLocal)
                        {
                            lock (TransactionLock)
                            {
                                RunStartHooks();

                                foreach (Node.Target target in newTransaction.TargetsToActivate)
                                {
                                    target.IsActivated = true;
                                }

                                newTransaction.Close(true);
                            }
                        }
                        else
                        {
                            newTransaction.Close(false);
                        }
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
                    SetCurrentTransaction(null, createLocal);
                }
            }
        }

        internal static T Apply<T>(Func<Transaction, T> code, bool createLocal)
        {
            Transaction localTransaction = null;

            bool createLocalNow = false;
            if (Monitor.TryEnter(TransactionLock))
            {
                try
                {
                    //if lock was obtained and we have a regular transaction, use it
                    if (currentTransaction != null)
                    {
                        return Apply(currentTransaction, false, code);
                    }

                    //check if we have a local transaction
                    localTransaction = LocalTransaction.Value;

                    //if lock was obtained and we have a local transaction, we will use it outside of the critical section
                    if (localTransaction == null)
                    {
                        //if lock was obtained and we do not have either a regular transaction or a local transaction and we are looking to create a regular transaction,
                        //create a regular transaction
                        if (!createLocal)
                        {
                            return Apply(null, false, code);
                        }
                        //if lock was obtained and we do not have either a regular transaction or a local transaction and we are looking to create a local transaction,
                        //create a local transaction as soon as we leave the critical section
                        else
                        {
                            createLocalNow = true;
                        }
                    }
                }
                finally
                {
                    Monitor.Exit(TransactionLock);
                }
            }
            //if lock was obtained and we do not have either a regular transaction or a local transaction and we are looking to create a local transaction,
            //create a local transaction
            if (createLocalNow)
            {
                return Apply(null, true, code);
            }

            //if lock was not obtained, we still need to check if we have a local transaction
            //if lock was obtained and we found a local transaction, we can skip this check
            if (localTransaction == null)
            {
                localTransaction = LocalTransaction.Value;
            }

            //if we have a local transaction, use it
            if (localTransaction != null)
            {
                return Apply(localTransaction, true, code);
            }

            //if lock was obtained, we will have returned by now

            //if lock was not obtained and we do not have either a regular transaction or a local transaction and we are looking to create a local transaction,
            //create a local transaction
            if (createLocal)
            {
                return Apply(null, true, code);
            }

            //if lock was not obtained and we do not have either a regular transaction or a local transaction and we are looking to create a regular transaction,
            //create a regular transaction inside a critical section
            lock (TransactionLock)
            {
                return Apply(null, false, code);
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

        private static Transaction Start()
        {
            RunStartHooks();

            return new Transaction(false);
        }

        internal void Send(Action<Transaction> action)
        {
            this.sendQueue.Add(action);
        }

        internal void Prioritized(Node node, Action<Transaction> action)
        {
            Entry e = new Entry(node, action);
            lock (Node.NodeRanksLock)
            {
                this.prioritizedQueue.Enqueue(e, node.Rank);
            }
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
            if (this.postQueue.TryGetValue(index, out Action<Transaction> existing))
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
            Apply(trans => trans.Post(-1, _ => action()), false);
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
                lock (Node.NodeRanksLock)
                {
                    foreach (Entry e in this.entries)
                    {
                        this.prioritizedQueue.Enqueue(e, e.Node.Rank);
                    }
                }
            }
        }

        internal void Close(bool usingLocal)
        {
            // ReSharper disable once ForCanBeConvertedToForeach
            for (int i = 0; i < this.sendQueue.Count; i++)
            {
                this.sendQueue[i](this);
            }
            this.sendQueue.Clear();

            this.ReachedClose = true;

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

            // ReSharper disable once ForCanBeConvertedToForeach
            for (int i = 0; i < this.lastQueue.Count; i++)
            {
                this.lastQueue[i]();
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
                        Transaction transaction = new Transaction(false);
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
                LocalTransaction.Value = transaction;
            }
            else
            {
                currentTransaction = transaction;
            }
        }

        private class Entry
        {
            public readonly Node Node;
            public readonly Action<Transaction> Action;

            public Entry(Node node, Action<Transaction> action)
            {
                this.Node = node;
                this.Action = action;
            }
        }
    }
}