using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using Sodium.Functional;

namespace Sodium.Frp.Async
{
    /// <summary>
    ///     Shorthand for a strategy that doesn't care about the result — the result type is fixed to
    ///     <see cref="Unit" />.
    /// </summary>
    [PublicAPI]
    public abstract class AsyncConcurrencyStrategy<TInput, TState>
        : AsyncConcurrencyStrategy<TInput, Unit, TState>
    {
    }

    /// <summary>
    ///     Shorthand for a strategy that cares about neither the input nor the result — both are fixed to
    ///     <see cref="Unit" />.
    /// </summary>
    [PublicAPI]
    public abstract class AsyncConcurrencyStrategy<TState>
        : AsyncConcurrencyStrategy<Unit, Unit, TState>
    {
    }

    /// <summary>
    ///     Non-generic entry point for the built-in strategies (Parallel, Queue, QueuePerGroup,
    ///     SwitchLatest) — each cares only about scheduling, not about the call's
    ///     <c>TInput</c>/<c>TResult</c>, so both are fixed to <see cref="Unit" />. The actual scheduling
    ///     logic for each lives once, shared, in Sodium.Core.Frp.Async's internal
    ///     <c>AsyncConcurrencyStrategyFactory</c> (generic over the "don't care" type, since Core itself
    ///     has no dependency on Sodium.Functional and so no type of its own to fix it to) — the static
    ///     methods below just supply <see cref="Unit" /> as that type argument and hand back the result,
    ///     giving this C# wrapper's own consumers a version typed against <see cref="Unit" />, since
    ///     that's the type this wrapper already uses for e.g. <c>cancelAll</c>. This class itself, and
    ///     its shorthand base classes below (<see cref="AsyncConcurrencyStrategy{TState}" />,
    ///     <see cref="AsyncConcurrencyStrategy{TInput,TState}" />), are unrelated to that shared
    ///     factory — they exist purely so a consumer writing their own custom strategy against
    ///     <see cref="Unit" /> can subclass <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}" />
    ///     without spelling out <see cref="Unit" /> twice.
    /// </summary>
    [PublicAPI]
    public abstract class AsyncConcurrencyStrategy
        : AsyncConcurrencyStrategy<Unit, Unit, Unit>
    {
        private static readonly AsyncConcurrencyStrategyBase<Unit, Unit> ParallelInstance =
            AsyncConcurrencyStrategyFactory.Parallel(Unit.Value);

        private static readonly AsyncConcurrencyStrategyBase<Unit, Unit> QueueInstance =
            AsyncConcurrencyStrategyFactory.Queue<Unit>();

        private static readonly AsyncConcurrencyStrategyBase<Unit, Unit> SwitchLatestInstance =
            AsyncConcurrencyStrategyFactory.SwitchLatest<Unit>();

        /// <summary>Every firing starts its own operation immediately; results arrive in completion order.</summary>
        public static AsyncConcurrencyStrategyBase<Unit, Unit> Parallel() => ParallelInstance;

        /// <summary>At most one operation runs at a time; later firings queue and run in order.</summary>
        public static AsyncConcurrencyStrategyBase<Unit, Unit> Queue() => QueueInstance;

        /// <summary>
        ///     Entry point for a per-group queue: at most one operation runs at a time within a
        ///     group, but different groups run concurrently. Call
        ///     <see cref="QueuePerGroupHelper{TInput}.Create{TGroup}" /> on the result to supply the
        ///     grouping function — <typeparamref name="TInput" /> here is only what's needed to infer
        ///     that call's input type; the actual strategy is created by
        ///     <see cref="QueuePerGroupHelper{TInput}.Create{TGroup}" />.
        /// </summary>
        public static QueuePerGroupHelper<TInput> QueuePerGroup<TInput>() => QueuePerGroupHelper<TInput>.Instance;

        /// <summary>
        ///     Exists solely so <see cref="QueuePerGroup{TInput}" /> can infer
        ///     <typeparamref name="TInput" /> while leaving <c>TGroup</c> to be inferred separately, by
        ///     <see cref="Create{TGroup}" /> — C# can't infer two type parameters from two different
        ///     calls otherwise.
        /// </summary>
        [PublicAPI]
        public class QueuePerGroupHelper<TInput>
        {
            internal static readonly QueuePerGroupHelper<TInput> Instance = new();

            private QueuePerGroupHelper()
            {
            }

            /// <summary>
            ///     Builds a queue-per-group strategy: <paramref name="getGroup" /> assigns each input to a
            ///     group, and within a group, later firings queue behind earlier ones exactly like
            ///     <see cref="Queue" /> — but different groups don't wait on each other.
            /// </summary>
            /// <param name="getGroup">Computes the group key for an input value.</param>
            /// <param name="groupComparer">
            ///     Optional equality comparer for group keys; defaults to
            ///     <see cref="EqualityComparer{TGroup}.Default" />.
            /// </param>
            public AsyncConcurrencyStrategyBase<TInput, Unit> Create<TGroup>(
                Func<TInput, TGroup> getGroup,
                IEqualityComparer<TGroup>? groupComparer = null)
                where TGroup : notnull =>
                AsyncConcurrencyStrategyFactory.QueuePerGroup<Unit, TInput, TGroup>(
                    getGroup: getGroup,
                    groupComparer: groupComparer);
        }

        /// <summary>A new firing cancels whatever is currently in flight and takes its place.</summary>
        public static AsyncConcurrencyStrategyBase<Unit, Unit> SwitchLatest() => SwitchLatestInstance;
    }
}