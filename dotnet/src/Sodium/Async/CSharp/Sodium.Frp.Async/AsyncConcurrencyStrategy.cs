using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using Sodium.Functional;

namespace Sodium.Frp.Async;

/// <summary>
///     Non-generic entry point for the built-in strategies (Parallel, Queue, QueuePerGroup,
///     SwitchLatest) — each cares only about scheduling, not about the call's
///     <c>TInput</c>/<c>TResult</c>, so both are fixed to <see cref="Unit" />. This is this
///     project's own copy of the equivalent built-ins in Sodium.Core.Frp.Async: that project has no
///     dependency on Sodium.Functional, so it has no publicly-nameable "don't care" type to type
///     these against and keeps its own copy internal. Each language wrapper provides its own
///     public version instead, against whatever "don't care" type is natural for that language —
///     this one against <see cref="Unit" />, since that's the type this C# wrapper already uses for
///     e.g. <c>cancelAll</c>. Subclasses <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}" />
///     directly (skipping the shorthand base classes Core uses for the same purpose) purely to avoid
///     colliding with Core's own same-named, internal-but-IVT-visible shorthand classes.
/// </summary>
[PublicAPI]
public static class AsyncConcurrencyStrategy
{
    /// <summary>Every firing starts its own operation immediately; results arrive in completion order.</summary>
    public static AsyncConcurrencyStrategyBase<Unit, Unit> Parallel() => ParallelStrategy.Instance;

    /// <summary>At most one operation runs at a time; later firings queue and run in order.</summary>
    public static AsyncConcurrencyStrategyBase<Unit, Unit> Queue() => QueueStrategy.Instance;

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
            new QueuePerGroupStrategy<TInput, TGroup>(getGroup: getGroup, groupComparer: groupComparer);
    }

    /// <summary>A new firing cancels whatever is currently in flight and takes its place.</summary>
    public static AsyncConcurrencyStrategyBase<Unit, Unit> SwitchLatest() => SwitchLatestStrategy.Instance;

    private sealed class ParallelStrategy
        : AsyncConcurrencyStrategy<Unit, Unit, Unit>
    {
        internal static readonly ParallelStrategy Instance = new();

        protected override Unit CreateState() => Unit.Value;

        protected internal override IReadOnlyList<AsyncToStart<Unit>> Admit(
            Unit state,
            AsyncQueuedItem<Unit> incoming) =>
            new[] { new AsyncToStart<Unit>(incoming) };

        protected internal override AsyncStrategyResult<Unit> OnCompleted(
            Unit state,
            AsyncQueuedItem<Unit> item,
            AsyncOutcome<Unit> outcome) =>
            new(publish: true, next: AsyncStrategyResult<Unit>.None);
    }

    private sealed class QueueStrategy
        : AsyncConcurrencyStrategy<Unit, Unit, QueueStrategy.State>
    {
        internal static readonly QueueStrategy Instance = new();

        private QueueStrategy()
        {
        }

        /// <summary>Which item (if any) is currently running, and the backlog waiting behind it.</summary>
        public sealed class State
        {
            internal readonly Queue<AsyncQueuedItem<Unit>> Pending = new();

            internal bool Busy;
        }

        protected override State CreateState() => new();

        protected internal override IReadOnlyList<AsyncToStart<Unit>> Admit(
            State state,
            AsyncQueuedItem<Unit> incoming)
        {
            if (state.Busy)
            {
                // Stays visible as Queued; still cancellable while it waits.
                state.Pending.Enqueue(incoming);

                return AsyncStrategyResult<Unit>.None;
            }

            state.Busy = true;

            return new[] { new AsyncToStart<Unit>(incoming) };
        }

        protected internal override AsyncStrategyResult<Unit> OnCompleted(
            State state,
            AsyncQueuedItem<Unit> item,
            AsyncOutcome<Unit> outcome)
        {
            if (state.Pending.Count > 0)
            {
                AsyncQueuedItem<Unit> next = state.Pending.Dequeue();

                // If `next` was canceled while it sat here, the execution engine will notice
                // when promoting it and short-circuit straight to Outcome.Canceled(), which
                // calls back into OnCompleted and naturally dequeues whatever comes after it.
                return new AsyncStrategyResult<Unit>(
                    publish: true,
                    next: new[] { new AsyncToStart<Unit>(next) });
            }

            state.Busy = false;

            return new AsyncStrategyResult<Unit>(publish: true, next: AsyncStrategyResult<Unit>.None);
        }
    }

    /// <summary>
    ///     One independent <see cref="QueueStrategy" />-style queue per group, where inputs are assigned to a
    ///     group by a specified group selector. Normally created via
    ///     <see cref="QueuePerGroup{TInput}" />/<see cref="QueuePerGroupHelper{TInput}.Create{TGroup}" />
    ///     rather than directly.
    /// </summary>
    private sealed class QueuePerGroupStrategy<TInput, TGroup>
        : AsyncConcurrencyStrategy<TInput, Unit, QueuePerGroupStrategy<TInput, TGroup>.State>
        where TGroup : notnull
    {
        private readonly Func<TInput, TGroup> getGroup;
        private readonly IEqualityComparer<TGroup>? groupComparer;

        public QueuePerGroupStrategy(Func<TInput, TGroup> getGroup, IEqualityComparer<TGroup>? groupComparer)
        {
            this.getGroup = getGroup;
            this.groupComparer = groupComparer;
        }

        /// <summary>Per-group queue state, keyed by group — groups are added on first use and removed once idle.</summary>
        public sealed class State
        {
            public State(IEqualityComparer<TGroup>? groupComparer) =>
                this.Groups = new Dictionary<TGroup, GroupState>(groupComparer);

            internal readonly Dictionary<TGroup, GroupState> Groups;
        }

        internal sealed class GroupState
        {
            internal readonly Queue<AsyncQueuedItem<TInput>> Pending = new();

            internal bool Busy;
        }

        protected override State CreateState() => new(this.groupComparer);

        protected internal override IReadOnlyList<AsyncToStart<TInput>> Admit(
            State state,
            AsyncQueuedItem<TInput> incoming)
        {
            TGroup group = this.getGroup(incoming.Value);

            GroupState groupState;

            if (state.Groups.TryGetValue(key: group, value: out GroupState? gs))
            {
                groupState = gs;
            }
            else
            {
                groupState = new GroupState();
                state.Groups.Add(key: group, value: groupState);
            }

            if (groupState.Busy)
            {
                // Stays visible as Queued; still cancellable while it waits.
                groupState.Pending.Enqueue(incoming);

                return AsyncStrategyResult<TInput>.None;
            }

            groupState.Busy = true;

            return new[] { new AsyncToStart<TInput>(incoming) };
        }

        protected internal override AsyncStrategyResult<TInput> OnCompleted(
            State state,
            AsyncQueuedItem<TInput> item,
            AsyncOutcome<Unit> outcome)
        {
            TGroup group = this.getGroup(item.Value);

            GroupState groupState;

            if (state.Groups.TryGetValue(key: group, value: out GroupState? gs))
            {
                groupState = gs;
            }
            else
            {
                throw new Exception("Could not find group.");
            }

            if (groupState.Pending.Count > 0)
            {
                AsyncQueuedItem<TInput> next = groupState.Pending.Dequeue();

                // If `next` was canceled while it sat here, the execution engine will notice
                // when promoting it and short-circuit straight to Outcome.Canceled(), which
                // calls back into OnCompleted and naturally dequeues whatever comes after it.
                return new AsyncStrategyResult<TInput>(
                    publish: true,
                    next: new[] { new AsyncToStart<TInput>(next) });
            }

            groupState.Busy = false;
            state.Groups.Remove(group);

            return new AsyncStrategyResult<TInput>(publish: true, next: AsyncStrategyResult<TInput>.None);
        }
    }

    private sealed class SwitchLatestStrategy
        : AsyncConcurrencyStrategy<Unit, Unit, SwitchLatestStrategy.State>
    {
        internal static readonly SwitchLatestStrategy Instance = new();

        /// <summary>The currently in-flight item, if any — superseded and replaced by each new firing.</summary>
        public sealed class State
        {
            internal AsyncQueuedItem<Unit>? Active;
        }

        protected override State CreateState() => new();

        protected internal override IReadOnlyList<AsyncToStart<Unit>> Admit(
            State state,
            AsyncQueuedItem<Unit> incoming)
        {
            // Cancel the item we're superseding via its own cancellation — no parallel
            // CancellationTokenSource of our own to create, own, or dispose. Safe even if
            // that item already finished on its own.
            state.Active?.Cancel();
            state.Active = incoming;

            return new[] { new AsyncToStart<Unit>(incoming) };
        }

        protected internal override AsyncStrategyResult<Unit> OnCompleted(
            State state,
            AsyncQueuedItem<Unit> item,
            AsyncOutcome<Unit> outcome)
        {
            // Only publish if nothing newer has since superseded this run.
            bool isCurrent = state.Active != null && state.Active.Id == item.Id;

            // Drop the reference once the current run finishes, so we don't pin the last
            // QueuedItem (and its value) indefinitely after everything has gone idle.
            if (isCurrent)
            {
                state.Active = null;
            }

            return new AsyncStrategyResult<Unit>(publish: isCurrent, next: AsyncStrategyResult<Unit>.None);
        }
    }
}
