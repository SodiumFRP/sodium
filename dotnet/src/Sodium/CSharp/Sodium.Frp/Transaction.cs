using System;
using System.Runtime.CompilerServices;
using Sodium.Functional;

namespace Sodium.Frp
{
    /// <summary>
    ///     A class for managing transactions.
    /// </summary>
    /// <remarks>
    ///     <para>
    ///         Transactions are serialized process-wide: at most one runs at a time, however many threads are
    ///         involved. A thread starting a transaction blocks until any transaction running on another thread has
    ///         finished. This is deliberate. It is what makes a transaction atomic with respect to every other thread
    ///         - no observer can ever see the graph half-updated - and it keeps the order in which updates are applied
    ///         deterministic no matter how many threads are pushing values in. Sodium can therefore be used from
    ///         multiple threads without any additional synchronization of your own.
    ///     </para>
    ///     <para>
    ///         The cost of that guarantee is that the lock is held for the whole transaction, which includes every
    ///         listener callback it fires and any <see cref="Post" /> action it queues - those run while the
    ///         transaction is closing, still under the lock. While a callback runs, no other thread can begin a
    ///         transaction, so callbacks should return promptly. Hand long-running or blocking work off to another
    ///         thread rather than doing it inline, and note that a callback which blocks waiting on a thread that is
    ///         itself trying to start a transaction will deadlock.
    ///     </para>
    ///     <para>
    ///         Nesting is free. Starting a transaction while one is already running on the same thread joins the
    ///         running transaction rather than acquiring the lock again, so the primitives that create their own
    ///         transactions cost nothing extra inside <see cref="Run{T}" /> or <see cref="RunVoid" />.
    ///     </para>
    /// </remarks>
    public static class Transaction
    {
        /// <summary>
        ///     Return whether or not there is a current transaction.
        /// </summary>
        /// <returns><code>true</code> if there is a current transaction, <code>false</code> otherwise.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static bool IsActive() => TransactionInternal.HasCurrentTransaction();

        /// <summary>
        ///     Execute the specified action inside a single transaction.
        /// </summary>
        /// <param name="action">The action to execute.</param>
        /// <remarks>
        ///     In most cases this is not needed, because all primitives will create their own transaction automatically.
        ///     It is useful for running multiple reactive operations atomically.
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static void RunVoid(Action action) =>
            TransactionInternal.RunImpl(
                () =>
                {
                    action();
                    return Unit.Value;
                });

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
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static T Run<T>(Func<T> f) => TransactionInternal.RunImpl(f);

        /// <summary>
        ///     Add an action that will be executed whenever a transaction is started.
        /// </summary>
        /// <param name="action"></param>
        /// <remarks>
        ///     The action may start transactions itself, which will not cause the hooks to execute recursively.
        ///     The main use case of this is for the implementation of a time/alarm system.
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static void OnStart(Action action) => TransactionInternal.OnStartImpl(action);

        /// <summary>
        ///     Execute an action after the current transaction is closed
        ///     or immediately if there is no current transaction.
        /// </summary>
        /// <param name="action">
        ///     The action to run after the current transaction is closed
        ///     or immediately if there is no current transaction.
        /// </param>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static void Post(Action action) => TransactionInternal.PostImpl(action);
    }
}