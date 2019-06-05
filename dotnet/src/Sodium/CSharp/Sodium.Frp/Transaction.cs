using System;
using System.Runtime.CompilerServices;
using Sodium.Functional;

namespace Sodium.Frp
{
    /// <summary>
    ///     A class for managing transactions.
    /// </summary>
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