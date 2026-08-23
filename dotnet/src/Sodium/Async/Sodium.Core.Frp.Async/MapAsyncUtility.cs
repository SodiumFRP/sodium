using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.ExceptionServices;
using System.Threading;
using System.Threading.Tasks;
using JetBrains.Annotations;
using Sodium.Functional;

namespace Sodium.Frp.Async
{
    /// <summary>Whether a tracked item is waiting for a slot or actually executing.</summary>
    [PublicAPI]
    public enum AsyncItemStatus
    {
        /// <summary>Admitted and tracked, but not yet promoted to Running by a strategy.</summary>
        Queued,

        /// <summary>Promoted by a strategy; its operation has been (or is about to be) invoked.</summary>
        Running
    }

    /// <summary>An input value being tracked by a MapAsync pipeline, and its current status.</summary>
    [PublicAPI]
    public readonly struct AsyncItem<TInput>
    {
        public AsyncItem(TInput value, AsyncItemStatus status)
        {
            this.Value = value;
            this.Status = status;
        }

        /// <summary>The original input value from the source stream.</summary>
        public TInput Value { get; }

        /// <summary>Whether this value is still waiting for a strategy to promote it, or already running.</summary>
        public AsyncItemStatus Status { get; }
    }

    /// <summary>
    ///     The observable status of a MapAsync pipeline: whether anything is actively running, and
    ///     every input value currently tracked (queued or running) along with its status. Also, the
    ///     sole handle for tearing the pipeline down — see <see cref="Dispose" />.
    /// </summary>
    [PublicAPI]
    public readonly struct AsyncMapStatus<TInput>
        : IDisposable
    {
        private readonly Action dispose;

        internal AsyncMapStatus(
            Cell<bool> isRunning,
            Cell<IReadOnlyList<AsyncItem<TInput>>> items,
            Action dispose)
        {
            this.IsRunning = isRunning;
            this.Items = items;
            this.dispose = dispose;
        }

        /// <summary>True while at least one item has Status == Running. Queued-only items don't count.</summary>
        public Cell<bool> IsRunning { get; }

        /// <summary>
        ///     Every value currently tracked, queued or running. Order is unspecified but each
        ///     update is a consistent snapshot.
        /// </summary>
        public Cell<IReadOnlyList<AsyncItem<TInput>>> Items { get; }

        /// <summary>
        ///     Stops this pipeline. No further values from the source stream are ever admitted
        ///     again — not queued, not started. Whether already-tracked items (queued or running)
        ///     are also canceled was fixed once, at setup, by MapAsync's cancelOnDispose parameter
        ///     (true by default) — it isn't a choice made here, since this parameterless
        ///     IDisposable.Dispose() is deliberately the only way to dispose. When that setup-time
        ///     choice was true, disposing cancels everything currently tracked, queued or running —
        ///     exactly as if a cancelAll stream had fired once, with the same caveats: a running
        ///     operation only actually stops if it observes its CancellationToken, and a queued item
        ///     is removed once the strategy would otherwise have promoted it, not necessarily the
        ///     instant this call returns. Safe to call more than once — later calls are no-ops.
        /// </summary>
        public void Dispose() => this.dispose();
    }

    /// <summary>
    ///     Shared base for the two halves of a MapAsync pipeline — the strategy
    ///     (<see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}" />) and the engine that runs
    ///     it (<see cref="AsyncMapExecutionManager{TInput,TResult,TStrategyInput,TStrategyResult}" />). Its only
    ///     purpose is to hold the small data types they pass back and forth — <see cref="AsyncQueuedItem{TInput}" />,
    ///     <see cref="AsyncToStart{TInput}" />, <see cref="AsyncOutcome{TResult}" />,
    ///     <see cref="AsyncStrategyResult{TInput}" /> — as nested types here rather than as public
    ///     top-level types. Neither class is a subtype of the other, so without this common base, at
    ///     least one side would need these types to be fully public just to name them; with it, both
    ///     reach them via ordinary inheritance while they stay off the library's public surface,
    ///     visible only to code that's either in this assembly or subclassing
    ///     <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}" /> to write a custom strategy.
    ///     Deliberately not generic itself — TInput/TResult belong to whichever of these nested types
    ///     actually needs them, not to every user of this base.
    /// </summary>
    [PublicAPI]
    public abstract class AsyncMapBase
    {
        // Prevents any type outside this assembly from deriving from AsyncMapBase directly.
        // AsyncConcurrencyStrategy<TInput,TResult,TState> — the type external code is meant to
        // subclass for a custom strategy — is itself in this assembly and can call this fine;
        // external subclasses of THAT class never need to call this constructor themselves.
        internal AsyncMapBase()
        {
        }

        /// <summary>
        ///     A value a MapAsync pipeline is tracking, from the moment it's admitted until it's
        ///     promoted, completed, or canceled — and the single object that identifies it
        ///     throughout, in both <see cref="AsyncToStart{TInput}" /> and
        ///     <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.OnCompleted" />. Opaque by
        ///     design: a strategy can hold onto one (typically in its per-call state, to promote
        ///     later, or to recognize it again on completion) and read its Value, but can't construct
        ///     one — the constructor is internal and the class is sealed, so every instance
        ///     originates from an
        ///     <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.Admit" /> call. Identity is
        ///     reference identity: the instance itself IS the id, so comparing two with
        ///     ReferenceEquals (or ==) tells you whether they're the same admitted value.
        /// </summary>
        [PublicAPI]
        protected internal sealed class AsyncQueuedItem<TInput>
        {
            internal AsyncQueuedItem(Guid id, TInput value, CancellationTokenSource cancellation)
            {
                this.Id = id;
                this.Value = value;
                this.Cancellation = cancellation;
            }

            /// <summary>
            ///     This item's identity, assigned once at admission — including across the separate
            ///     <see cref="AsyncQueuedItem{TInput}" /> instances (one typed for the strategy, one for
            ///     the public <see cref="AsyncItem{TInput}" /> view) the execution engine keeps for a
            ///     single admitted value. Equal ID means the same tracked value.
            /// </summary>
            public Guid Id { get; }

            /// <summary>The value this item was admitted with.</summary>
            public TInput Value { get; }

            /// <summary>
            ///     The source of this item's own cancellation — what <see cref="Cancel" /> cancels, and
            ///     what the execution engine links into the operation's token. Internal: a strategy
            ///     cancels through <see cref="Cancel" /> rather than touching this directly.
            /// </summary>
            internal CancellationTokenSource Cancellation { get; }

            /// <summary>
            ///     Cancels this specific tracked item — the same mechanism a cancelAll/cancelMatching
            ///     stream uses, available to a strategy for its own scheduling decisions (e.g.
            ///     SwitchLatest superseding its previous run). Works whether this item is still
            ///     Queued or already Running: a queued item is simply never started when its turn
            ///     comes, and a running one only actually stops if its operation observes the
            ///     CancellationToken it was given. Either way the item completes normally, as
            ///     Canceled, so <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.OnCompleted" /> still
            ///     runs for it and can chain to whatever's next.
            ///     Safe to call on an item that has already completed, already been canceled, or is
            ///     mid-completion — those are no-ops rather than errors, so a strategy holding a stale
            ///     reference doesn't have to track precisely when an item stopped being cancellable.
            /// </summary>
            public void Cancel()
            {
                // The execution engine disposes an item's CancellationTokenSource once it's done, so
                // a stale reference can land here after disposal. Cancel() on a disposed CTS throws,
                // and "already finished" is exactly the case a strategy shouldn't have to guard
                // against, so swallow just that.
                try
                {
                    this.Cancellation.Cancel();
                }
                catch (ObjectDisposedException)
                {
                }
            }
        }

        /// <summary>A previously-tracked item to start (or promote from Queued to Running) right now.</summary>
        [PublicAPI]
        protected internal readonly struct AsyncToStart<TInput>
        {
            /// <exception cref="ArgumentNullException"><paramref name="item" /> is null.</exception>
            public AsyncToStart(AsyncQueuedItem<TInput> item, CancellationToken strategyToken = default)
            {
                this.Item = item ?? throw new ArgumentNullException(nameof(item));
                this.StrategyToken = strategyToken;
            }

            /// <summary>The previously-admitted item to start or promote — from an earlier Admit call.</summary>
            public AsyncQueuedItem<TInput> Item { get; }

            /// <summary>
            ///     Optional extra cancellation source to link into this run, on top of the item's own.
            ///     Not needed to cancel an item the strategy is managing — use
            ///     <see cref="AsyncQueuedItem{TInput}.Cancel" /> for that. This is for tying a run to
            ///     something external instead: a per-request timeout, an ambient operation token, a
            ///     shared token covering a batch of work. Leave it defaulted otherwise.
            /// </summary>
            public CancellationToken StrategyToken { get; }
        }

        /// <summary>How an item finished.</summary>
        [PublicAPI]
        protected internal readonly struct AsyncOutcome<TResult>
        {
            /// <summary>Which of the three ways this item finished.</summary>
            private readonly AsyncOutcomeKind kind;

            /// <summary>The operation's return value, if <see cref="kind" /> is Succeeded; default otherwise.</summary>
            private readonly TResult? value;

            /// <summary>The exception the operation threw, if <see cref="kind" /> is Failed; null otherwise.</summary>
            private readonly Exception? error;

            private AsyncOutcome(AsyncOutcomeKind kind, TResult? value, Exception? error)
            {
                this.kind = kind;
                this.value = value;
                this.error = error;
            }

            public T Match<T>(
                Func<TResult, T> onSucceeded,
                Func<Exception, T> onFailed,
                Func<T> onCanceled) =>
                this.kind switch
                {
                    AsyncOutcomeKind.Succeeded => onSucceeded(this.value!),
                    AsyncOutcomeKind.Failed => onFailed(this.error!),
                    AsyncOutcomeKind.Canceled => onCanceled(),
                    _ => throw new InvalidOperationException("Unknown value for kind.")
                };

            public void MatchVoid(
                Action<TResult>? onSucceeded,
                Action<Exception>? onFailed,
                Action? onCanceled)
            {
                switch (this.kind)
                {
                    case AsyncOutcomeKind.Succeeded:
                        onSucceeded?.Invoke(this.value!);
                        break;
                    case AsyncOutcomeKind.Failed:
                        onFailed?.Invoke(this.error!);
                        break;
                    case AsyncOutcomeKind.Canceled:
                        onCanceled?.Invoke();
                        break;
                    default:
                        throw new InvalidOperationException("Unknown value for kind.");
                }
            }

            public async Task<T> MatchAsync<T>(
                Func<TResult, Task<T>> onSucceeded,
                Func<Exception, Task<T>> onFailed,
                Func<Task<T>> onCanceled) =>
                this.kind switch
                {
                    AsyncOutcomeKind.Succeeded => await onSucceeded(this.value!),
                    AsyncOutcomeKind.Failed => await onFailed(this.error!),
                    AsyncOutcomeKind.Canceled => await onCanceled(),
                    _ => throw new InvalidOperationException("Unknown value for kind.")
                };

            public async Task MatchAsyncVoid(
                Func<TResult, Task>? onSucceeded,
                Func<Exception, Task>? onFailed,
                Func<Task>? onCanceled)
            {
                switch (this.kind)
                {
                    case AsyncOutcomeKind.Succeeded:
                        if (onSucceeded != null)
                        {
                            await onSucceeded(this.value!);
                        }

                        break;
                    case AsyncOutcomeKind.Failed:
                        if (onFailed != null)
                        {
                            await onFailed(this.error!);
                        }

                        break;
                    case AsyncOutcomeKind.Canceled:
                        if (onCanceled != null)
                        {
                            await onCanceled();
                        }

                        break;
                    default:
                        throw new InvalidOperationException("Unknown value for kind.");
                }
            }

            /// <summary>Builds a Succeeded outcome carrying the operation's return value.</summary>
            public static AsyncOutcome<TResult> Succeeded(TResult value) =>
                new(kind: AsyncOutcomeKind.Succeeded, value: value, error: null);

            /// <summary>Builds a Failed outcome carrying the exception the operation threw.</summary>
            public static AsyncOutcome<TResult> Failed(Exception error) =>
                new(kind: AsyncOutcomeKind.Failed, value: default, error: error);

            /// <summary>Builds a Canceled outcome.</summary>
            public static AsyncOutcome<TResult> Canceled() =>
                new(kind: AsyncOutcomeKind.Canceled, value: default, error: null);

            /// <summary>The three ways an item can finish.</summary>
            private enum AsyncOutcomeKind
            {
                /// <summary>The operation returned a value — see <see cref="AsyncOutcome{TResult}.value" />.</summary>
                Succeeded,

                /// <summary>The operation threw — see <see cref="AsyncOutcome{TResult}.error" />.</summary>
                Failed,

                /// <summary>
                ///     The operation was canceled (or never started because it was canceled while still
                ///     Queued). Never published regardless of what OnCompleted returns — see
                ///     <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.OnCompleted" />.
                /// </summary>
                Canceled
            }
        }

        /// <summary>
        ///     A strategy's answer: whether to publish the just-finished outcome, and what to start next.
        /// </summary>
        [PublicAPI]
        protected internal readonly struct AsyncStrategyResult<TInput>
        {
            /// <summary>An empty Next list — nothing more to start as a result of this decision.</summary>
            public static readonly IReadOnlyList<AsyncToStart<TInput>> None = Array.Empty<AsyncToStart<TInput>>();

            public AsyncStrategyResult(bool publish, IReadOnlyList<AsyncToStart<TInput>> next)
            {
                this.Publish = publish;
                this.Next = next;
            }

            /// <summary>Whether the just-finished outcome should be sent to results/errors.</summary>
            public bool Publish { get; }

            /// <summary>Previously-tracked items to start (or promote) right now, as a consequence.</summary>
            public IReadOnlyList<AsyncToStart<TInput>> Next { get; }
        }

        /// <summary>
        ///     Type-erases a strategy's state so
        ///     <see cref="AsyncMapExecutionManager{TInput,TResult,TStrategyInput,TStrategyResult}" /> can
        ///     hold "a strategy plus its state" without itself being generic over
        ///     state — which is exactly what keeps TState from ever appearing
        ///     in a MapAsync signature. See <see cref="StateManager{TInput,TResult,TState}" /> for the
        ///     sole implementation.
        /// </summary>
        internal interface IStateManager<TInput, TResult>
        {
            /// <summary>Forwards to <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.Admit" /> with the closed-over state.</summary>
            IReadOnlyList<AsyncToStart<TInput>> Admit(AsyncQueuedItem<TInput> incoming);

            /// <summary>
            ///     Forwards to <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.OnCompleted" /> with the closed-over
            ///     state.
            /// </summary>
            AsyncStrategyResult<TInput> OnCompleted(AsyncQueuedItem<TInput> item, AsyncOutcome<TResult> outcome);
        }

        /// <summary>
        ///     Pairs a strategy instance with the one <typeparamref name="TState" /> created for a
        ///     single MapAsync call (see <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.CreateStateManager" />),
        ///     so the execution engine can call Admit/OnCompleted without knowing
        ///     <typeparamref name="TState" /> itself.
        /// </summary>
        internal class StateManager<TInput, TResult, TState>
            : IStateManager<TInput, TResult>
        {
            private readonly AsyncConcurrencyStrategy<TInput, TResult, TState> strategy;
            private readonly TState state;

            public StateManager(AsyncConcurrencyStrategy<TInput, TResult, TState> strategy, TState state)
            {
                this.strategy = strategy;
                this.state = state;
            }

            public IReadOnlyList<AsyncToStart<TInput>> Admit(AsyncQueuedItem<TInput> incoming) =>
                this.strategy.Admit(state: this.state, incoming: incoming);

            public AsyncStrategyResult<TInput> OnCompleted(
                AsyncQueuedItem<TInput> item,
                AsyncOutcome<TResult> outcome) =>
                this.strategy.OnCompleted(state: this.state, item: item, outcome: outcome);
        }
    }

    /// <summary>
    ///     Extension methods that bridge an impure asynchronous operation into the FRP world:
    ///     listen on a Stream&lt;TInput&gt;, run an async operation per firing, push the result into a
    ///     StreamSink&lt;TResult&gt;, and expose what's queued/running — optionally wired up to streams
    ///     that trigger cancellation of queued or running work alike. The returned
    ///     <see cref="AsyncMapStatus{TInput}" /> is IDisposable; disposing it is how you tear the whole
    ///     pipeline down.
    /// </summary>
    [PublicAPI]
    internal static class AsyncStreamUtility
    {
        /// <summary>
        ///     Convenience overload for a strategy that only cares about scheduling, not about
        ///     <typeparamref name="TInput" />/<typeparamref name="TResult" /> themselves (e.g. Parallel,
        ///     Queue, SwitchLatest) — both are erased to <see cref="Unit" /> before reaching it. See the
        ///     canonical
        ///     <see
        ///         cref="MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload
        ///     for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">
        ///     The type carried by the source stream — the input each invocation of
        ///     <paramref name="operation" /> receives, and the type reported by
        ///     <see cref="AsyncMapStatus{TInput}.Items" /> and matched against by
        ///     <paramref name="cancelMatching" />.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type <paramref name="operation" /> produces on success, sent to
        ///     <paramref name="results" />.
        /// </typeparam>
        /// <param name="source">
        ///     The stream of inputs to run against. Every firing is offered to
        ///     <paramref name="strategy" />, which decides whether it starts immediately or waits.
        ///     After the returned status is disposed, further firings are ignored entirely.
        /// </param>
        /// <param name="results">
        ///     Required. Each successful operation's return value is sent here, in completion order
        ///     rather than the order inputs arrived. A result whose run was superseded or canceled
        ///     is not sent — see <paramref name="strategy" />.
        /// </param>
        /// <param name="errors">
        ///     Required. Every failed operation is sent here — there is deliberately no way to call
        ///     this method without somewhere for errors to go.
        /// </param>
        /// <param name="operation">
        ///     The asynchronous work to run per input. It's invoked inline, so it doesn't reach a
        ///     thread pool until it awaits something itself, and it's handed a CancellationToken
        ///     combining this item's own cancellation with any token the strategy supplied. Honoring
        ///     that token is what makes <paramref name="cancelAll" />, <paramref name="cancelMatching" />,
        ///     and <paramref name="cancelOnDispose" /> take effect on work that has already started —
        ///     an operation that ignores it still runs to completion, and cancellation only means its
        ///     result goes unpublished. There is deliberately no overload that omits the token.
        /// </param>
        /// <param name="strategy">
        ///     How overlapping requests are handled — a strategy that only cares about scheduling, not
        ///     about <typeparamref name="TInput" />/<typeparamref name="TResult" /> themselves. A
        ///     strategy instance holds no state of its own and may safely be reused across multiple
        ///     MapAsync calls (even concurrently) — each call gets its own freshly created state
        ///     manager, so separate pipelines never share scheduling state.
        /// </param>
        /// <param name="cancelAll">
        ///     Optional. Each firing cancels every tracked operation — queued or already running.
        ///     A queued item that's canceled is simply never started when its turn comes.
        ///     Cancellation only takes effect for a running operation if it observes its
        ///     CancellationToken.
        /// </param>
        /// <param name="cancelMatching">
        ///     Optional. Each firing cancels whichever tracked operations (queued or running) were
        ///     admitted for an input value present in the fired collection (compared with the
        ///     default equality comparer for TInput). Same caveats as <paramref name="cancelAll" />.
        /// </param>
        /// <param name="cancelOnDispose">
        ///     Whether disposing the returned <see cref="AsyncMapStatus{TInput}" /> also cancels every
        ///     item tracked at that point (queued or running) — true by default. Either way,
        ///     disposing always, unconditionally, stops any further values from ever being admitted.
        ///     This is fixed here, at setup, rather than being a parameter of Dispose itself, since
        ///     IDisposable.Dispose() is deliberately the only way to dispose.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsyncImpl<TInput, TResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<Unit, Unit> strategy,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true) =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: _ => Unit.Value,
                resultConverter: _ => Unit.Value,
                cancelAll: cancelAll,
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload for a strategy that cares about <typeparamref name="TStrategyInput" />
        ///     but not about the result (<see cref="Unit" />), where <typeparamref name="TInput" /> is
        ///     already a <typeparamref name="TStrategyInput" /> (e.g. QueuePerGroup on the call's own
        ///     input type). See the canonical
        ///     <see
        ///         cref="MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the
        ///     full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">
        ///     The type carried by the source stream — the input each invocation of
        ///     <paramref name="operation" /> receives, and the type reported by
        ///     <see cref="AsyncMapStatus{TInput}.Items" /> and matched against by
        ///     <paramref name="cancelMatching" />.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type <paramref name="operation" /> produces on success, sent to
        ///     <paramref name="results" />.
        /// </typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Usually the same as
        ///     <typeparamref name="TInput" />, but a strategy can be written against a broader type
        ///     (an interface or base class) so a single instance can be shared across MapAsync calls
        ///     for several different, more specific <typeparamref name="TInput" /> types — the
        ///     <c>where TInput : TStrategyInput</c> constraint is what permits that while still
        ///     letting the strategy operate purely in terms of <typeparamref name="TStrategyInput" />.
        /// </typeparam>
        /// <param name="source">
        ///     The stream of inputs to run against. Every firing is offered to
        ///     <paramref name="strategy" />, which decides whether it starts immediately or waits.
        ///     After the returned status is disposed, further firings are ignored entirely.
        /// </param>
        /// <param name="results">
        ///     Required. Each successful operation's return value is sent here, in completion order
        ///     rather than the order inputs arrived. A result whose run was superseded or canceled
        ///     is not sent — see <paramref name="strategy" />.
        /// </param>
        /// <param name="errors">
        ///     Required. Every failed operation is sent here — there is deliberately no way to call
        ///     this method without somewhere for errors to go.
        /// </param>
        /// <param name="operation">
        ///     The asynchronous work to run per input. It's invoked inline, so it doesn't reach a
        ///     thread pool until it awaits something itself, and it's handed a CancellationToken
        ///     combining this item's own cancellation with any token the strategy supplied. Honoring
        ///     that token is what makes <paramref name="cancelAll" />, <paramref name="cancelMatching" />,
        ///     and <paramref name="cancelOnDispose" /> take effect on work that has already started —
        ///     an operation that ignores it still runs to completion, and cancellation only means its
        ///     result goes unpublished. There is deliberately no overload that omits the token.
        /// </param>
        /// <param name="strategy">
        ///     How overlapping requests are handled — a strategy that cares about
        ///     <typeparamref name="TStrategyInput" /> but not about the result (fixed to
        ///     <see cref="Unit" />). A strategy instance holds no state of its own and may safely be
        ///     reused across multiple MapAsync calls (even concurrently) — each call gets its own
        ///     freshly created state manager, so separate pipelines never share scheduling state.
        /// </param>
        /// <param name="cancelAll">
        ///     Optional. Each firing cancels every tracked operation — queued or already running.
        ///     A queued item that's canceled is simply never started when its turn comes.
        ///     Cancellation only takes effect for a running operation if it observes its
        ///     CancellationToken.
        /// </param>
        /// <param name="cancelMatching">
        ///     Optional. Each firing cancels whichever tracked operations (queued or running) were
        ///     admitted for an input value present in the fired collection (compared with the
        ///     default equality comparer for TInput). Same caveats as <paramref name="cancelAll" />.
        /// </param>
        /// <param name="cancelOnDispose">
        ///     Whether disposing the returned <see cref="AsyncMapStatus{TInput}" /> also cancels every
        ///     item tracked at that point (queued or running) — true by default. Either way,
        ///     disposing always, unconditionally, stops any further values from ever being admitted.
        ///     This is fixed here, at setup, rather than being a parameter of Dispose itself, since
        ///     IDisposable.Dispose() is deliberately the only way to dispose.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsyncImpl<TInput, TResult, TStrategyInput>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, Unit> strategy,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
            where TInput : TStrategyInput =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: v => v,
                resultConverter: _ => Unit.Value,
                cancelAll: cancelAll,
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload for a strategy that cares about <typeparamref name="TStrategyInput" />
        ///     but not about the result (<see cref="Unit" />), where <paramref name="inputConverter" />
        ///     derives it from <typeparamref name="TInput" /> (e.g. QueuePerGroup, deriving the group
        ///     key). See the canonical
        ///     <see
        ///         cref="MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the
        ///     full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">
        ///     The type carried by the source stream — the input each invocation of
        ///     <paramref name="operation" /> receives, and the type reported by
        ///     <see cref="AsyncMapStatus{TInput}.Items" /> and matched against by
        ///     <paramref name="cancelMatching" />.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type <paramref name="operation" /> produces on success, sent to
        ///     <paramref name="results" />.
        /// </typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TInput" /> by inheritance at all, since
        ///     <paramref name="inputConverter" /> derives it explicitly.
        /// </typeparam>
        /// <param name="source">
        ///     The stream of inputs to run against. Every firing is offered to
        ///     <paramref name="strategy" />, which decides whether it starts immediately or waits.
        ///     After the returned status is disposed, further firings are ignored entirely.
        /// </param>
        /// <param name="results">
        ///     Required. Each successful operation's return value is sent here, in completion order
        ///     rather than the order inputs arrived. A result whose run was superseded or canceled
        ///     is not sent — see <paramref name="strategy" />.
        /// </param>
        /// <param name="errors">
        ///     Required. Every failed operation is sent here — there is deliberately no way to call
        ///     this method without somewhere for errors to go.
        /// </param>
        /// <param name="operation">
        ///     The asynchronous work to run per input. It's invoked inline, so it doesn't reach a
        ///     thread pool until it awaits something itself, and it's handed a CancellationToken
        ///     combining this item's own cancellation with any token the strategy supplied. Honoring
        ///     that token is what makes <paramref name="cancelAll" />, <paramref name="cancelMatching" />,
        ///     and <paramref name="cancelOnDispose" /> take effect on work that has already started —
        ///     an operation that ignores it still runs to completion, and cancellation only means its
        ///     result goes unpublished. There is deliberately no overload that omits the token.
        /// </param>
        /// <param name="strategy">
        ///     How overlapping requests are handled — a strategy that cares about
        ///     <typeparamref name="TStrategyInput" /> but not about the result (fixed to
        ///     <see cref="Unit" />). A strategy instance holds no state of its own and may safely be
        ///     reused across multiple MapAsync calls (even concurrently) — each call gets its own
        ///     freshly created state manager, so separate pipelines never share scheduling state.
        /// </param>
        /// <param name="inputConverter">
        ///     Converts each <typeparamref name="TInput" /> value to the
        ///     <typeparamref name="TStrategyInput" /> <paramref name="strategy" /> is written against,
        ///     before it's admitted (e.g. deriving a QueuePerGroup group key).
        /// </param>
        /// <param name="cancelAll">
        ///     Optional. Each firing cancels every tracked operation — queued or already running.
        ///     A queued item that's canceled is simply never started when its turn comes.
        ///     Cancellation only takes effect for a running operation if it observes its
        ///     CancellationToken.
        /// </param>
        /// <param name="cancelMatching">
        ///     Optional. Each firing cancels whichever tracked operations (queued or running) were
        ///     admitted for an input value present in the fired collection (compared with the
        ///     default equality comparer for TInput). Same caveats as <paramref name="cancelAll" />.
        /// </param>
        /// <param name="cancelOnDispose">
        ///     Whether disposing the returned <see cref="AsyncMapStatus{TInput}" /> also cancels every
        ///     item tracked at that point (queued or running) — true by default. Either way,
        ///     disposing always, unconditionally, stops any further values from ever being admitted.
        ///     This is fixed here, at setup, rather than being a parameter of Dispose itself, since
        ///     IDisposable.Dispose() is deliberately the only way to dispose.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsyncImpl<TInput, TResult, TStrategyInput>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, Unit> strategy,
            Func<TInput, TStrategyInput> inputConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true) =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: inputConverter,
                resultConverter: _ => Unit.Value,
                cancelAll: cancelAll,
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload for a strategy that doesn't care about the input (<see cref="Unit" />)
        ///     but does care about <typeparamref name="TStrategyResult" />, where
        ///     <typeparamref name="TResult" /> is already a <typeparamref name="TStrategyResult" />. See
        ///     the canonical
        ///     <see
        ///         cref="MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">
        ///     The type carried by the source stream — the input each invocation of
        ///     <paramref name="operation" /> receives, and the type reported by
        ///     <see cref="AsyncMapStatus{TInput}.Items" /> and matched against by
        ///     <paramref name="cancelMatching" />.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type <paramref name="operation" /> produces on success, sent to
        ///     <paramref name="results" />.
        /// </typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Usually the same as
        ///     <typeparamref name="TResult" />, but a strategy can be written against a broader type
        ///     (an interface or base class) so a single instance can be shared across MapAsync calls
        ///     for several different, more specific <typeparamref name="TResult" /> types — the
        ///     <c>where TResult : TStrategyResult</c> constraint is what permits that while still
        ///     letting the strategy operate purely in terms of <typeparamref name="TStrategyResult" />.
        /// </typeparam>
        /// <param name="source">
        ///     The stream of inputs to run against. Every firing is offered to
        ///     <paramref name="strategy" />, which decides whether it starts immediately or waits.
        ///     After the returned status is disposed, further firings are ignored entirely.
        /// </param>
        /// <param name="results">
        ///     Required. Each successful operation's return value is sent here, in completion order
        ///     rather than the order inputs arrived. A result whose run was superseded or canceled
        ///     is not sent — see <paramref name="strategy" />.
        /// </param>
        /// <param name="errors">
        ///     Required. Every failed operation is sent here — there is deliberately no way to call
        ///     this method without somewhere for errors to go.
        /// </param>
        /// <param name="operation">
        ///     The asynchronous work to run per input. It's invoked inline, so it doesn't reach a
        ///     thread pool until it awaits something itself, and it's handed a CancellationToken
        ///     combining this item's own cancellation with any token the strategy supplied. Honoring
        ///     that token is what makes <paramref name="cancelAll" />, <paramref name="cancelMatching" />,
        ///     and <paramref name="cancelOnDispose" /> take effect on work that has already started —
        ///     an operation that ignores it still runs to completion, and cancellation only means its
        ///     result goes unpublished. There is deliberately no overload that omits the token.
        /// </param>
        /// <param name="strategy">
        ///     How overlapping requests are handled — a strategy that doesn't care about the input
        ///     (fixed to <see cref="Unit" />) but does care about <typeparamref name="TStrategyResult" />.
        ///     A strategy instance holds no state of its own and may safely be reused across multiple
        ///     MapAsync calls (even concurrently) — each call gets its own freshly created state
        ///     manager, so separate pipelines never share scheduling state.
        /// </param>
        /// <param name="cancelAll">
        ///     Optional. Each firing cancels every tracked operation — queued or already running.
        ///     A queued item that's canceled is simply never started when its turn comes.
        ///     Cancellation only takes effect for a running operation if it observes its
        ///     CancellationToken.
        /// </param>
        /// <param name="cancelMatching">
        ///     Optional. Each firing cancels whichever tracked operations (queued or running) were
        ///     admitted for an input value present in the fired collection (compared with the
        ///     default equality comparer for TInput). Same caveats as <paramref name="cancelAll" />.
        /// </param>
        /// <param name="cancelOnDispose">
        ///     Whether disposing the returned <see cref="AsyncMapStatus{TInput}" /> also cancels every
        ///     item tracked at that point (queued or running) — true by default. Either way,
        ///     disposing always, unconditionally, stops any further values from ever being admitted.
        ///     This is fixed here, at setup, rather than being a parameter of Dispose itself, since
        ///     IDisposable.Dispose() is deliberately the only way to dispose.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsyncImpl<TInput, TResult, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<Unit, TStrategyResult> strategy,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
            where TResult : TStrategyResult =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: _ => Unit.Value,
                resultConverter: v => v,
                cancelAll: cancelAll,
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload for a strategy that doesn't care about the input (<see cref="Unit" />)
        ///     but does care about <typeparamref name="TStrategyResult" />, where
        ///     <paramref name="resultConverter" /> derives it from <typeparamref name="TResult" />. See
        ///     the canonical
        ///     <see
        ///         cref="MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">
        ///     The type carried by the source stream — the input each invocation of
        ///     <paramref name="operation" /> receives, and the type reported by
        ///     <see cref="AsyncMapStatus{TInput}.Items" /> and matched against by
        ///     <paramref name="cancelMatching" />.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type <paramref name="operation" /> produces on success, sent to
        ///     <paramref name="results" />.
        /// </typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TResult" /> by inheritance at all, since
        ///     <paramref name="resultConverter" /> derives it explicitly.
        /// </typeparam>
        /// <param name="source">
        ///     The stream of inputs to run against. Every firing is offered to
        ///     <paramref name="strategy" />, which decides whether it starts immediately or waits.
        ///     After the returned status is disposed, further firings are ignored entirely.
        /// </param>
        /// <param name="results">
        ///     Required. Each successful operation's return value is sent here, in completion order
        ///     rather than the order inputs arrived. A result whose run was superseded or canceled
        ///     is not sent — see <paramref name="strategy" />.
        /// </param>
        /// <param name="errors">
        ///     Required. Every failed operation is sent here — there is deliberately no way to call
        ///     this method without somewhere for errors to go.
        /// </param>
        /// <param name="operation">
        ///     The asynchronous work to run per input. It's invoked inline, so it doesn't reach a
        ///     thread pool until it awaits something itself, and it's handed a CancellationToken
        ///     combining this item's own cancellation with any token the strategy supplied. Honoring
        ///     that token is what makes <paramref name="cancelAll" />, <paramref name="cancelMatching" />,
        ///     and <paramref name="cancelOnDispose" /> take effect on work that has already started —
        ///     an operation that ignores it still runs to completion, and cancellation only means its
        ///     result goes unpublished. There is deliberately no overload that omits the token.
        /// </param>
        /// <param name="strategy">
        ///     How overlapping requests are handled — a strategy that doesn't care about the input
        ///     (fixed to <see cref="Unit" />) but does care about <typeparamref name="TStrategyResult" />.
        ///     A strategy instance holds no state of its own and may safely be reused across multiple
        ///     MapAsync calls (even concurrently) — each call gets its own freshly created state
        ///     manager, so separate pipelines never share scheduling state.
        /// </param>
        /// <param name="resultConverter">
        ///     Converts each successful <typeparamref name="TResult" /> to the
        ///     <typeparamref name="TStrategyResult" /> <paramref name="strategy" /> is written against,
        ///     before <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.OnCompleted" /> sees it.
        /// </param>
        /// <param name="cancelAll">
        ///     Optional. Each firing cancels every tracked operation — queued or already running.
        ///     A queued item that's canceled is simply never started when its turn comes.
        ///     Cancellation only takes effect for a running operation if it observes its
        ///     CancellationToken.
        /// </param>
        /// <param name="cancelMatching">
        ///     Optional. Each firing cancels whichever tracked operations (queued or running) were
        ///     admitted for an input value present in the fired collection (compared with the
        ///     default equality comparer for TInput). Same caveats as <paramref name="cancelAll" />.
        /// </param>
        /// <param name="cancelOnDispose">
        ///     Whether disposing the returned <see cref="AsyncMapStatus{TInput}" /> also cancels every
        ///     item tracked at that point (queued or running) — true by default. Either way,
        ///     disposing always, unconditionally, stops any further values from ever being admitted.
        ///     This is fixed here, at setup, rather than being a parameter of Dispose itself, since
        ///     IDisposable.Dispose() is deliberately the only way to dispose.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsyncImpl<TInput, TResult, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<Unit, TStrategyResult> strategy,
            Func<TResult, TStrategyResult> resultConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true) =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: _ => Unit.Value,
                resultConverter: resultConverter,
                cancelAll: cancelAll,
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload where <typeparamref name="TStrategyInput" /> is already a <typeparamref name="TInput" /> and
        ///     <typeparamref name="TResult" /> is already a <typeparamref name="TStrategyResult" />. See the canonical
        ///     <see
        ///         cref="MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <summary>
        ///     Runs <paramref name="operation" /> for each firing of <paramref name="source" />,
        ///     sending successes to <paramref name="results" /> and failures to <paramref name="errors" />.
        /// </summary>
        /// <typeparam name="TInput">
        ///     The type carried by the source stream — the input each invocation of
        ///     <paramref name="operation" /> receives, and the type reported by
        ///     <see cref="AsyncMapStatus{TInput}.Items" /> and matched against by
        ///     <paramref name="cancelMatching" />.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type <paramref name="operation" /> produces on success, sent to
        ///     <paramref name="results" />.
        /// </typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Usually the same as
        ///     <typeparamref name="TInput" />, but a strategy can be written against a broader type
        ///     (an interface or base class) so a single instance can be shared across MapAsync calls
        ///     for several different, more specific <typeparamref name="TInput" /> types — the
        ///     <c>where TInput : TStrategyInput</c> constraint is what permits that while still
        ///     letting the strategy operate purely in terms of <typeparamref name="TStrategyInput" />.
        /// </typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against, in the same spirit as
        ///     <typeparamref name="TStrategyInput" /> — inferred, usually equal to
        ///     <typeparamref name="TResult" />, and related to it by the
        ///     <c>where TResult : TStrategyResult</c> constraint.
        /// </typeparam>
        /// <param name="source">
        ///     The stream of inputs to run against. Every firing is offered to
        ///     <paramref name="strategy" />, which decides whether it starts immediately or waits.
        ///     After the returned status is disposed, further firings are ignored entirely.
        /// </param>
        /// <param name="results">
        ///     Required. Each successful operation's return value is sent here, in completion order
        ///     rather than the order inputs arrived. A result whose run was superseded or canceled
        ///     is not sent — see <paramref name="strategy" />.
        /// </param>
        /// <param name="errors">
        ///     Required. Every failed operation is sent here — there is deliberately no way to call
        ///     this method without somewhere for errors to go.
        /// </param>
        /// <param name="operation">
        ///     The asynchronous work to run per input. It's invoked inline, so it doesn't reach a
        ///     thread pool until it awaits something itself, and it's handed a CancellationToken
        ///     combining this item's own cancellation with any token the strategy supplied. Honoring
        ///     that token is what makes <paramref name="cancelAll" />, <paramref name="cancelMatching" />,
        ///     and <paramref name="cancelOnDispose" /> take effect on work that has already started —
        ///     an operation that ignores it still runs to completion, and cancellation only means its
        ///     result goes unpublished. There is deliberately no overload that omits the token.
        /// </param>
        /// <param name="strategy">
        ///     How overlapping requests are handled. A strategy instance holds no state of its own
        ///     and may safely be reused across multiple MapAsync calls (even concurrently) — each
        ///     call gets its own freshly created state manager, so separate pipelines never share scheduling state.
        /// </param>
        /// <param name="cancelAll">
        ///     Optional. Each firing cancels every tracked operation — queued or already running.
        ///     A queued item that's canceled is simply never started when its turn comes.
        ///     Cancellation only takes effect for a running operation if it observes its
        ///     CancellationToken.
        /// </param>
        /// <param name="cancelMatching">
        ///     Optional. Each firing cancels whichever tracked operations (queued or running) were
        ///     admitted for an input value present in the fired collection (compared with the
        ///     default equality comparer for TInput). Same caveats as <paramref name="cancelAll" />.
        /// </param>
        /// <param name="cancelOnDispose">
        ///     Whether disposing the returned <see cref="AsyncMapStatus{TInput}" /> also cancels every
        ///     item tracked at that point (queued or running) — true by default. Either way,
        ///     disposing always, unconditionally, stops any further values from ever being admitted.
        ///     This is fixed here, at setup, rather than being a parameter of Dispose itself, since
        ///     IDisposable.Dispose() is deliberately the only way to dispose.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsyncImpl<TInput, TResult, TStrategyInput, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, TStrategyResult> strategy,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
            where TInput : TStrategyInput
            where TResult : TStrategyResult =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: v => v,
                resultConverter: v => v,
                cancelAll: cancelAll,
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload where <paramref name="inputConverter" /> derives
        ///     <typeparamref name="TStrategyInput" /> from <typeparamref name="TInput" />, but
        ///     <typeparamref name="TResult" /> is already a <typeparamref name="TStrategyResult" />. See
        ///     the canonical
        ///     <see
        ///         cref="MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">
        ///     The type carried by the source stream — the input each invocation of
        ///     <paramref name="operation" /> receives, and the type reported by
        ///     <see cref="AsyncMapStatus{TInput}.Items" /> and matched against by
        ///     <paramref name="cancelMatching" />.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type <paramref name="operation" /> produces on success, sent to
        ///     <paramref name="results" />.
        /// </typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TInput" /> by inheritance at all, since
        ///     <paramref name="inputConverter" /> derives it explicitly.
        /// </typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against, inferred from
        ///     <paramref name="strategy" />'s type. Usually the same as <typeparamref name="TResult" />,
        ///     but a strategy can be written against a broader type so a single instance can be shared
        ///     across MapAsync calls for several different, more specific <typeparamref name="TResult" />
        ///     types — the <c>where TResult : TStrategyResult</c> constraint is what permits that.
        /// </typeparam>
        /// <param name="source">
        ///     The stream of inputs to run against. Every firing is offered to
        ///     <paramref name="strategy" />, which decides whether it starts immediately or waits.
        ///     After the returned status is disposed, further firings are ignored entirely.
        /// </param>
        /// <param name="results">
        ///     Required. Each successful operation's return value is sent here, in completion order
        ///     rather than the order inputs arrived. A result whose run was superseded or canceled
        ///     is not sent — see <paramref name="strategy" />.
        /// </param>
        /// <param name="errors">
        ///     Required. Every failed operation is sent here — there is deliberately no way to call
        ///     this method without somewhere for errors to go.
        /// </param>
        /// <param name="operation">
        ///     The asynchronous work to run per input. It's invoked inline, so it doesn't reach a
        ///     thread pool until it awaits something itself, and it's handed a CancellationToken
        ///     combining this item's own cancellation with any token the strategy supplied. Honoring
        ///     that token is what makes <paramref name="cancelAll" />, <paramref name="cancelMatching" />,
        ///     and <paramref name="cancelOnDispose" /> take effect on work that has already started —
        ///     an operation that ignores it still runs to completion, and cancellation only means its
        ///     result goes unpublished. There is deliberately no overload that omits the token.
        /// </param>
        /// <param name="strategy">
        ///     How overlapping requests are handled. A strategy instance holds no state of its own
        ///     and may safely be reused across multiple MapAsync calls (even concurrently) — each
        ///     call gets its own freshly created state manager, so separate pipelines never share
        ///     scheduling state.
        /// </param>
        /// <param name="inputConverter">
        ///     Converts each <typeparamref name="TInput" /> value to the
        ///     <typeparamref name="TStrategyInput" /> <paramref name="strategy" /> is written against,
        ///     before it's admitted.
        /// </param>
        /// <param name="cancelAll">
        ///     Optional. Each firing cancels every tracked operation — queued or already running.
        ///     A queued item that's canceled is simply never started when its turn comes.
        ///     Cancellation only takes effect for a running operation if it observes its
        ///     CancellationToken.
        /// </param>
        /// <param name="cancelMatching">
        ///     Optional. Each firing cancels whichever tracked operations (queued or running) were
        ///     admitted for an input value present in the fired collection (compared with the
        ///     default equality comparer for TInput). Same caveats as <paramref name="cancelAll" />.
        /// </param>
        /// <param name="cancelOnDispose">
        ///     Whether disposing the returned <see cref="AsyncMapStatus{TInput}" /> also cancels every
        ///     item tracked at that point (queued or running) — true by default. Either way,
        ///     disposing always, unconditionally, stops any further values from ever being admitted.
        ///     This is fixed here, at setup, rather than being a parameter of Dispose itself, since
        ///     IDisposable.Dispose() is deliberately the only way to dispose.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsyncImpl<TInput, TResult, TStrategyInput, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, TStrategyResult> strategy,
            Func<TInput, TStrategyInput> inputConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
            where TResult : TStrategyResult =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: inputConverter,
                resultConverter: v => v,
                cancelAll: cancelAll,
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload where <typeparamref name="TInput" /> is already a
        ///     <typeparamref name="TStrategyInput" />, but <paramref name="resultConverter" /> derives
        ///     <typeparamref name="TStrategyResult" /> from <typeparamref name="TResult" />. See the
        ///     canonical
        ///     <see
        ///         cref="MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload
        ///     for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">
        ///     The type carried by the source stream — the input each invocation of
        ///     <paramref name="operation" /> receives, and the type reported by
        ///     <see cref="AsyncMapStatus{TInput}.Items" /> and matched against by
        ///     <paramref name="cancelMatching" />.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type <paramref name="operation" /> produces on success, sent to
        ///     <paramref name="results" />.
        /// </typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against, inferred from
        ///     <paramref name="strategy" />'s type. Usually the same as <typeparamref name="TInput" />,
        ///     but a strategy can be written against a broader type so a single instance can be shared
        ///     across MapAsync calls for several different, more specific <typeparamref name="TInput" />
        ///     types — the <c>where TInput : TStrategyInput</c> constraint is what permits that.
        /// </typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TResult" /> by inheritance at all, since
        ///     <paramref name="resultConverter" /> derives it explicitly.
        /// </typeparam>
        /// <param name="source">
        ///     The stream of inputs to run against. Every firing is offered to
        ///     <paramref name="strategy" />, which decides whether it starts immediately or waits.
        ///     After the returned status is disposed, further firings are ignored entirely.
        /// </param>
        /// <param name="results">
        ///     Required. Each successful operation's return value is sent here, in completion order
        ///     rather than the order inputs arrived. A result whose run was superseded or canceled
        ///     is not sent — see <paramref name="strategy" />.
        /// </param>
        /// <param name="errors">
        ///     Required. Every failed operation is sent here — there is deliberately no way to call
        ///     this method without somewhere for errors to go.
        /// </param>
        /// <param name="operation">
        ///     The asynchronous work to run per input. It's invoked inline, so it doesn't reach a
        ///     thread pool until it awaits something itself, and it's handed a CancellationToken
        ///     combining this item's own cancellation with any token the strategy supplied. Honoring
        ///     that token is what makes <paramref name="cancelAll" />, <paramref name="cancelMatching" />,
        ///     and <paramref name="cancelOnDispose" /> take effect on work that has already started —
        ///     an operation that ignores it still runs to completion, and cancellation only means its
        ///     result goes unpublished. There is deliberately no overload that omits the token.
        /// </param>
        /// <param name="strategy">
        ///     How overlapping requests are handled. A strategy instance holds no state of its own
        ///     and may safely be reused across multiple MapAsync calls (even concurrently) — each
        ///     call gets its own freshly created state manager, so separate pipelines never share
        ///     scheduling state.
        /// </param>
        /// <param name="resultConverter">
        ///     Converts each successful <typeparamref name="TResult" /> to the
        ///     <typeparamref name="TStrategyResult" /> <paramref name="strategy" /> is written against,
        ///     before <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.OnCompleted" /> sees it.
        /// </param>
        /// <param name="cancelAll">
        ///     Optional. Each firing cancels every tracked operation — queued or already running.
        ///     A queued item that's canceled is simply never started when its turn comes.
        ///     Cancellation only takes effect for a running operation if it observes its
        ///     CancellationToken.
        /// </param>
        /// <param name="cancelMatching">
        ///     Optional. Each firing cancels whichever tracked operations (queued or running) were
        ///     admitted for an input value present in the fired collection (compared with the
        ///     default equality comparer for TInput). Same caveats as <paramref name="cancelAll" />.
        /// </param>
        /// <param name="cancelOnDispose">
        ///     Whether disposing the returned <see cref="AsyncMapStatus{TInput}" /> also cancels every
        ///     item tracked at that point (queued or running) — true by default. Either way,
        ///     disposing always, unconditionally, stops any further values from ever being admitted.
        ///     This is fixed here, at setup, rather than being a parameter of Dispose itself, since
        ///     IDisposable.Dispose() is deliberately the only way to dispose.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsyncImpl<TInput, TResult, TStrategyInput, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, TStrategyResult> strategy,
            Func<TResult, TStrategyResult> resultConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
            where TInput : TStrategyInput =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: v => v,
                resultConverter: resultConverter,
                cancelAll: cancelAll,
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     The fully general form every other MapAsync
        ///     overload eventually forwards to, and the only one that needs no relationship at all
        ///     between <typeparamref name="TInput" />/<typeparamref name="TResult" /> and
        ///     <typeparamref name="TStrategyInput" />/<typeparamref name="TStrategyResult" /> — both
        ///     <paramref name="inputConverter" /> and <paramref name="resultConverter" /> are supplied
        ///     explicitly instead. Prefer one of the narrower overloads where it fits (they exist
        ///     precisely so most calls don't need to write out both converters); reach for this one
        ///     when <paramref name="strategy" /> is written against types unrelated by inheritance to
        ///     this call's own <typeparamref name="TInput" />/<typeparamref name="TResult" /> in both
        ///     directions at once.
        /// </summary>
        /// <typeparam name="TInput">
        ///     The type carried by the source stream — the input each invocation of
        ///     <paramref name="operation" /> receives, and the type reported by
        ///     <see cref="AsyncMapStatus{TInput}.Items" /> and matched against by
        ///     <paramref name="cancelMatching" />.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type <paramref name="operation" /> produces on success, sent to
        ///     <paramref name="results" />.
        /// </typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TInput" /> by inheritance at all, since
        ///     <paramref name="inputConverter" /> derives it explicitly.
        /// </typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TResult" /> by inheritance at all, since
        ///     <paramref name="resultConverter" /> derives it explicitly.
        /// </typeparam>
        /// <param name="source">
        ///     The stream of inputs to run against. Every firing is offered to
        ///     <paramref name="strategy" />, which decides whether it starts immediately or waits.
        ///     After the returned status is disposed, further firings are ignored entirely.
        /// </param>
        /// <param name="results">
        ///     Required. Each successful operation's return value is sent here, in completion order
        ///     rather than the order inputs arrived. A result whose run was superseded or canceled
        ///     is not sent — see <paramref name="strategy" />.
        /// </param>
        /// <param name="errors">
        ///     Required. Every failed operation is sent here — there is deliberately no way to call
        ///     this method without somewhere for errors to go.
        /// </param>
        /// <param name="operation">
        ///     The asynchronous work to run per input. It's invoked inline, so it doesn't reach a
        ///     thread pool until it awaits something itself, and it's handed a CancellationToken
        ///     combining this item's own cancellation with any token the strategy supplied. Honoring
        ///     that token is what makes <paramref name="cancelAll" />, <paramref name="cancelMatching" />,
        ///     and <paramref name="cancelOnDispose" /> take effect on work that has already started —
        ///     an operation that ignores it still runs to completion, and cancellation only means its
        ///     result goes unpublished. There is deliberately no overload that omits the token.
        /// </param>
        /// <param name="strategy">
        ///     How overlapping requests are handled. A strategy instance holds no state of its own
        ///     and may safely be reused across multiple MapAsync calls (even concurrently) — each
        ///     call gets its own freshly created state manager, so separate pipelines never share
        ///     scheduling state.
        /// </param>
        /// <param name="inputConverter">
        ///     Converts each <typeparamref name="TInput" /> value to the
        ///     <typeparamref name="TStrategyInput" /> <paramref name="strategy" /> is written against,
        ///     before it's admitted.
        /// </param>
        /// <param name="resultConverter">
        ///     Converts each successful <typeparamref name="TResult" /> to the
        ///     <typeparamref name="TStrategyResult" /> <paramref name="strategy" /> is written against,
        ///     before <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.OnCompleted" /> sees it.
        /// </param>
        /// <param name="cancelAll">
        ///     Optional. Each firing cancels every tracked operation — queued or already running.
        ///     A queued item that's canceled is simply never started when its turn comes.
        ///     Cancellation only takes effect for a running operation if it observes its
        ///     CancellationToken.
        /// </param>
        /// <param name="cancelMatching">
        ///     Optional. Each firing cancels whichever tracked operations (queued or running) were
        ///     admitted for an input value present in the fired collection (compared with the
        ///     default equality comparer for TInput). Same caveats as <paramref name="cancelAll" />.
        /// </param>
        /// <param name="cancelOnDispose">
        ///     Whether disposing the returned <see cref="AsyncMapStatus{TInput}" /> also cancels every
        ///     item tracked at that point (queued or running) — true by default. Either way,
        ///     disposing always, unconditionally, stops any further values from ever being admitted.
        ///     This is fixed here, at setup, rather than being a parameter of Dispose itself, since
        ///     IDisposable.Dispose() is deliberately the only way to dispose.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsyncImpl<TInput, TResult, TStrategyInput, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, TStrategyResult> strategy,
            Func<TInput, TStrategyInput> inputConverter,
            Func<TResult, TStrategyResult> resultConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
        {
            if (source is null)
            {
                throw new ArgumentNullException(nameof(source));
            }

            if (results is null)
            {
                throw new ArgumentNullException(nameof(results));
            }

            if (errors is null)
            {
                throw new ArgumentNullException(nameof(errors));
            }

            if (operation is null)
            {
                throw new ArgumentNullException(nameof(operation));
            }

            if (strategy is null)
            {
                throw new ArgumentNullException(nameof(strategy));
            }

            return new AsyncMapExecutionManager<TInput, TResult, TStrategyInput, TStrategyResult>(
                strategy: strategy,
                inputConverter: inputConverter,
                resultConverter: resultConverter,
                results: results,
                errors: errors,
                operation: operation,
                cancelAll: cancelAll,
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose).Attach(source);
        }
    }

    /// <summary>
    ///     The non-generic-over-<c>TState</c> face of a strategy — what
    ///     <see
    ///         cref="AsyncStreamUtility.MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
    ///     and its overloads actually accept. This is what keeps a strategy's <c>TState</c> out of every
    ///     MapAsync signature: callers and the execution engine only ever see
    ///     <see cref="AsyncConcurrencyStrategyBase{TInput,TResult}" />, never
    ///     <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}" /> itself. Sealed off from direct
    ///     external subclassing (internal constructor) — write a custom strategy by subclassing
    ///     <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}" /> instead.
    /// </summary>
    [PublicAPI]
    public abstract class AsyncConcurrencyStrategyBase<TInput, TResult>
        : AsyncMapBase
    {
        internal AsyncConcurrencyStrategyBase()
        {
        }

        /// <summary>
        ///     Creates the <see cref="AsyncMapBase.IStateManager{TInput,TResult}" /> for one MapAsync call — see
        ///     <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.CreateState" />.
        /// </summary>
        internal abstract IStateManager<TInput, TResult> CreateStateManager();
    }

    /// <summary>
    ///     Base class for a MapAsync scheduling strategy: how a stream of async requests is
    ///     admitted and sequenced. A strategy answers two questions, both as plain data —
    ///     <see cref="Admit" /> ("given this newly-tracked value, what do I start now?") and
    ///     <see cref="OnCompleted" /> ("given this outcome, what do I start next, and should it be
    ///     published?") — and does so purely in terms of a <typeparamref name="TState" /> it manages
    ///     itself. A strategy instance holds no state of its own: everything mutable lives in
    ///     <typeparamref name="TState" />, one instance of which is created per MapAsync call via
    ///     <see cref="CreateState" />. This is what makes a strategy instance safely reusable across
    ///     multiple, even concurrent, MapAsync calls — the execution engine (see
    ///     <see
    ///         cref="AsyncStreamUtility.MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
    ///     ) owns the
    ///     per-call <typeparamref name="TState" /> and is the only thing that ever passes it back in. Neither
    ///     <see cref="Admit" /> nor <see cref="OnCompleted" /> can touch the result/error sinks or a
    ///     Task directly, or start one — they just describe what should happen and the execution
    ///     engine carries it out. The one imperative affordance is
    ///     <see cref="AsyncMapBase.AsyncQueuedItem{TInput}.Cancel" />, for cancelling an item the
    ///     strategy is managing — it doesn't publish anything or start anything, it just routes into
    ///     the same cancellation path an external cancelAll stream uses, so the item still completes
    ///     through <see cref="OnCompleted" /> like any other. A completed item's identity is just the
    ///     same item the strategy already saw in <see cref="Admit" /> — there's no separate handle to
    ///     plumb through; hold onto the item itself (in <typeparamref name="TState" />) if you need
    ///     to recognize it again later.
    /// </summary>
    [PublicAPI]
    public abstract class AsyncConcurrencyStrategy<TInput, TResult, TState>
        : AsyncConcurrencyStrategyBase<TInput, TResult>
    {
        internal override IStateManager<TInput, TResult> CreateStateManager() =>
            new StateManager<TInput, TResult, TState>(strategy: this, state: this.CreateState());

        /// <summary>
        ///     Creates a fresh, independent scheduling state for one MapAsync call. Called exactly
        ///     once per call — see
        ///     <see
        ///         cref="AsyncStreamUtility.MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     — so separate pipelines using the same strategy instance never see each other's state,
        ///     even if they run concurrently.
        /// </summary>
        protected abstract TState CreateState();

        /// <summary>
        ///     Given a newly admitted value (already tracked as Queued — see
        ///     <see cref="AsyncMapBase.AsyncQueuedItem{TInput}" />), which item(s) should start right now? Always
        ///     called from inside a Sodium transaction — see
        ///     <see cref="AsyncMapExecutionManager{TInput,TResult,TStrategyInput,TStrategyResult}" /> for why that makes
        ///     mutating <paramref name="state" /> here safe without an explicit lock. Return an
        ///     <see cref="AsyncMapBase.AsyncToStart{TInput}" /> wrapping <paramref name="incoming" /> to start it
        ///     immediately; omitting it leaves it Queued. If you leave it Queued, hold onto
        ///     <paramref name="incoming" /> itself (not just its Value) in <paramref name="state" />
        ///     if you ever want to start it later — that's what preserves its identity and
        ///     cancellation across the wait, and it's the same value you'll be handed back in
        ///     <see cref="OnCompleted" />.
        /// </summary>
        protected internal abstract IReadOnlyList<AsyncToStart<TInput>> Admit(
            TState state,
            AsyncQueuedItem<TInput> incoming);

        /// <summary>
        ///     Given the outcome of a finished item, should it be published, and which
        ///     previously-tracked item(s) — if any — should start right now as a consequence (e.g. the next
        ///     queued item)? Always called from inside a Sodium transaction, same as <see cref="Admit" />.
        ///     Each <see cref="AsyncMapBase.AsyncToStart{TInput}" /> returned here must wrap an
        ///     <see cref="AsyncMapBase.AsyncQueuedItem{TInput}" /> you already hold from an earlier
        ///     <see cref="Admit" /> call — there's no way to introduce a value that wasn't previously
        ///     admitted. <paramref name="item" /> is the exact same instance this strategy was handed
        ///     for this value back in <see cref="Admit" /> — the same instance, so ReferenceEquals
        ///     against anything you stashed away in <paramref name="state" /> answers "is this still
        ///     the current run?". Canceled outcomes are never published regardless of what's returned
        ///     here — cancellation (external, a strategy superseding its own prior run, or a queued
        ///     item canceled before its turn came) is always treated as an expected, silent
        ///     completion.
        /// </summary>
        protected internal abstract AsyncStrategyResult<TInput> OnCompleted(
            TState state,
            AsyncQueuedItem<TInput> item,
            AsyncOutcome<TResult> outcome);
    }

    /// <summary>
    ///     Shorthand for a strategy that doesn't publish a meaningful result — the result type is fixed
    ///     to <see cref="Unit" />.
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
    ///     <c>TInput</c>/<c>TResult</c>, so both are fixed to <see cref="Unit" /> (or, for
    ///     QueuePerGroup, just the input type needed to compute a group key).
    /// </summary>
    [PublicAPI]
    public abstract class AsyncConcurrencyStrategy
        : AsyncConcurrencyStrategy<Unit>
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
            : AsyncConcurrencyStrategy
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
            : AsyncConcurrencyStrategy<QueueStrategy.State>
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
        ///     One independent <see cref="QueueStrategy" />-style queue per group, where inputs are assigned to a group by a
        ///     specified group selector. Normally created via
        ///     <see cref="QueuePerGroup{TInput}" />/<see cref="QueuePerGroupHelper{TInput}.Create{TGroup}" />
        ///     rather than directly.
        /// </summary>
        private sealed class QueuePerGroupStrategy<TInput, TGroup>
            : AsyncConcurrencyStrategy<TInput, QueuePerGroupStrategy<TInput, TGroup>.State>
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
            : AsyncConcurrencyStrategy<SwitchLatestStrategy.State>
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

    /// <summary>
    ///     Runs one MapAsync pipeline: starting operations, catching exceptions, routing
    ///     results/errors, tracking queued/running items, wiring up external cancellation (including
    ///     cancelling something before it ever starts), and managing transaction boundaries. This is
    ///     everything a <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}" /> is deliberately
    ///     not trusted with — a strategy only ever answers Admit/OnCompleted as data; this class is
    ///     what carries the answer out. One instance is created per MapAsync call (see
    ///     <see
    ///         cref="AsyncStreamUtility.MapAsyncImpl{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
    ///     ), owns that call's single state (created once, up front, and is never
    ///     shared between calls — which is what lets the same strategy instance be reused safely
    ///     across many calls at once. It shares <see cref="AsyncMapBase" /> with
    ///     <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}" /> purely so it can reach that
    ///     class's nested data types (<see cref="AsyncMapBase.AsyncQueuedItem{TInput}" /> and friends)
    ///     despite not being a subtype of it — the two are otherwise unrelated. Internally, everything
    ///     the strategy touches — tracked items, ToStart, Outcome — is expressed in terms of
    ///     <typeparamref name="TStrategyInput" />/<typeparamref name="TStrategyResult" /> rather than
    ///     <typeparamref name="TInput" />/<typeparamref name="TResult" />, since that's what the
    ///     strategy is written against; only <typeparamref name="TInput" /> values ever actually flow
    ///     in (from <c>source</c>) and only <typeparamref name="TResult" /> values ever actually flow
    ///     out (to <c>results</c>), so converting between them at those two edges —
    ///     <see cref="inputConverter" /> on the way in, <see cref="resultConverter" /> on the way out —
    ///     is this class's job, not the strategy's. Since an arbitrary
    ///     <see cref="inputConverter" /> generally can't be converted back, the original
    ///     <typeparamref name="TInput" />/<typeparamref name="TResult" /> values are never recovered
    ///     from their converted counterparts — they're carried alongside them instead (see
    ///     <see cref="Entry" /> and the <c>value</c>/<c>result</c> parameters threaded through
    ///     <see cref="PromoteAndLaunch" />/<see cref="StartOperation" />/<see cref="Complete" />).
    /// </summary>
    internal sealed class AsyncMapExecutionManager<TInput, TResult, TStrategyInput, TStrategyResult>
        : AsyncMapBase
    {
        private static readonly Mutation NoMutation =
            new(
                remove: Array.Empty<Guid>(),
                promote: Array.Empty<Guid>(),
                add: Array.Empty<Entry>());

        private readonly Func<TInput, TStrategyInput> inputConverter;

        private readonly Func<TResult, TStrategyResult> resultConverter;

        // Created once, up front — see the class remarks — and never replaced. No lock guards
        // it, and none is needed: it's only ever touched from inside a Sodium transaction
        // (Attach's Map, and Complete's Transaction.RunVoid), and Sodium serializes ALL
        // transactions process-wide behind one global lock — at most one is ever in progress
        // anywhere, on any thread. This relies specifically on that "one transaction at a time"
        // guarantee; if you're on a Sodium implementation that doesn't provide it, this state
        // would need its own lock again.
        private readonly IStateManager<TStrategyInput, TStrategyResult> stateManager;

        // Carries edits (add/promote/remove) to the tracked-items list. Always sent from either
        // Map's transform (not a registered Listen() callback — see Attach) or from well outside
        // any Sodium callback (background-thread continuations) — send() is legal in both cases.
        private readonly StreamSink<Mutation> mutations =
            StreamInternal.CreateSinkImpl<Mutation>(CombineMutations);

        // Always sent inside a transaction (from Dispose) — reuses the exact same
        // Snapshot(tracked)+Cancel() pattern as a user-supplied cancelAll stream. Wired up
        // unconditionally in Attach, regardless of whether the caller passed their own cancelAll.
        private readonly StreamSink<Unit> disposeCancelTrigger = StreamInternal.CreateSinkImpl<Unit>();

        private readonly Func<TInput, CancellationToken, Task<TResult>> operation;

        // Gates new admissions once disposed. Only ever read/written from inside a Sodium
        // transaction (see Attach and Dispose), so it relies on the same "one transaction at a
        // time" guarantee as everything else in this class rather than needing to be volatile.
        private bool disposed;

        // 0 = active, 1 = disposed. Guards Dispose's own idempotency — unlike disposed above,
        // Dispose can be called from any thread with no transaction open yet, so this needs its
        // own thread-safe check via Interlocked rather than relying on Sodium's serialization.
        private int disposeState;

        // These hold the strong reference each ListenWeak subscription needs to stay attached
        // (ListenWeak only keeps a weak reference on the source-stream side — see Attach for
        // why). As long as this execution manager itself is reachable, these fields keep the
        // subscriptions alive; once nothing references it, these go with it and the
        // subscriptions lapse on their own. Dispose additionally Unlistens them explicitly, for
        // immediate/deterministic detachment rather than waiting on GC.
        private IListener? cancelAllListener;

        private IListener? cancelMatchingListener;

        private IListener? disposeCancelListener;

        private readonly StreamSink<TResult> results;

        private readonly StreamSink<Exception> errors;

        private readonly Stream<Unit>? cancelAll;

        private readonly Stream<IReadOnlyCollection<TInput>>? cancelMatching;

        // Fixed once, at setup, and read only by Dispose — see MapAsync's cancelOnDispose
        // parameter. Not mutated after Attach, so no transaction/Interlocked protection needed.
        private readonly bool cancelOnDispose;

        internal AsyncMapExecutionManager(
            AsyncConcurrencyStrategyBase<TStrategyInput, TStrategyResult> strategy,
            Func<TInput, TStrategyInput> inputConverter,
            Func<TResult, TStrategyResult> resultConverter,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            Stream<Unit>? cancelAll,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching,
            bool cancelOnDispose)
        {
            this.inputConverter = inputConverter;
            this.resultConverter = resultConverter;
            this.stateManager = strategy.CreateStateManager();
            this.results = results;
            this.errors = errors;
            this.operation = operation;
            this.cancelAll = cancelAll;
            this.cancelMatching = cancelMatching;
            this.cancelOnDispose = cancelOnDispose;
        }

        internal AsyncMapStatus<TInput> Attach(Stream<TInput> source)
        {
            Cell<Entry[]> trackedCell =
                TransactionInternal.Apply((trans, _) =>
                {
                    LoopedCell<Dictionary<Guid, Entry>> entryByIdCellLoop = new();

                    // Map runs as ordinary transaction-processing code, not a registered Listen()
                    // callback, so it isn't subject to Sodium's "no send() inside a callback"
                    // restriction — and it fires in the SAME transaction as the source. Every admitted
                    // value is tracked from this moment on: it's added as Queued, then immediately
                    // promoted to Running for whichever ToStart(s) Admit returns (normally just the
                    // value itself, for the built-in strategies). Once disposed, this is a permanent
                    // no-op — Admit is never called again, so nothing new is ever queued or started.
                    Stream<Mutation> starts =
                        source
                            .SnapshotImpl(
                                c: entryByIdCellLoop,
                                f: (value, entryById) => (Value: value, EntryById: entryById))
                            .MapImpl(o =>
                            {
                                if (this.disposed)
                                {
                                    return NoMutation;
                                }

                                CancellationTokenSource cancellation = new();

                                Guid newEntryId = Guid.NewGuid();

                                // TInput becomes TStrategyInput here via inputConverter — the "in" edge;
                                // see the class remarks. The original TInput is preserved separately, in
                                // newEntry below, since inputConverter's result generally can't be
                                // converted back.
                                AsyncQueuedItem<TStrategyInput> incoming =
                                    new(
                                        value: this.inputConverter(o.Value),
                                        id: newEntryId,
                                        cancellation: cancellation);

                                Entry newEntry =
                                    new(
                                        item: new AsyncQueuedItem<TInput>(
                                            value: o.Value,
                                            id: newEntryId,
                                            cancellation: cancellation),
                                        status: AsyncItemStatus.Queued,
                                        value: o.Value);

                                IReadOnlyList<AsyncToStart<TStrategyInput>> toStart =
                                    this.stateManager.Admit(incoming: incoming);

                                Guid[] promote = new Guid[toStart.Count];

                                for (int i = 0; i < toStart.Count; i++)
                                {
                                    promote[i] = toStart[i].Item.Id;

                                    TInput value;

                                    if (newEntryId == promote[i])
                                    {
                                        value = o.Value;
                                    }
                                    else
                                    {
                                        if (o.EntryById.ContainsKey(promote[i]))
                                        {
                                            value = o.EntryById[promote[i]].Value;
                                        }
                                        else
                                        {
                                            throw new Exception("Could not find item to start.");
                                        }
                                    }

                                    this.PromoteAndLaunch(
                                        toStart: toStart[i],
                                        value: value,
                                        entryByIdCell: entryByIdCellLoop);
                                }

                                return new Mutation(
                                    remove: Array.Empty<Guid>(),
                                    promote: promote,
                                    add: new[] { newEntry });
                            });

                    Cell<Entry[]> trackedCell =
                        starts
                            .MergeImpl(s: this.mutations, f: CombineMutations)
                            .AccumImpl(
                                initialState: Array.Empty<Entry>(),
                                f: (mutation, list) =>
                                    Apply(list: list, mutation: mutation));

                    Cell<Dictionary<Guid, Entry>> entryByIdCell =
                        trackedCell.MapImpl(tracked => tracked.ToDictionary(e => e.Item.Id));

                    entryByIdCellLoop.Loop(trans: trans, c: entryByIdCell);

                    return trackedCell;
                });

            // Snapshot pairs each cancellation-trigger firing with `tracked`'s value from the
            // start of that same transaction, so cancelling and admitting a value in the exact
            // same transaction can't race each other. Cancel() itself is a plain BCL call, not
            // a Sodium send(), so it's unrestricted inside a Listen() callback. This reaches
            // queued items exactly the same way as running ones, since every tracked entry —
            // regardless of status — already has its own CancellationTokenSource from the
            // moment it was admitted.
            //
            // ListenWeak, not Listen: cancelAll/cancelMatching are supplied by the caller and
            // may well outlive any single MapAsync call (e.g. a "Cancel" stream shared across a
            // whole view). A strong Listen would mean the source stream holds this pipeline
            // (this execution manager, the result/error sinks, everything reachable from
            // `tracked`) alive forever, whether or not the caller still references
            // IsRunning/Items — Dispose would become the ONLY way to ever release it. With
            // ListenWeak, the subscription only survives as long as something else keeps this
            // execution manager reachable (normally the caller holding onto IsRunning/Items);
            // once nothing does, the whole thing becomes collectible together, and this callback
            // simply stops firing. This is a safety net for a forgotten Dispose, not a
            // replacement for calling it: GC timing is non-deterministic, and it does nothing
            // for a Task that's already running — that keeps executing to completion on its own
            // regardless of whether anything is still listening for cancellation.
            if (this.cancelAll != null)
            {
                this.cancelAllListener =
                    this.cancelAll
                        .SnapshotImpl(c: trackedCell, f: (_, entries) => entries)
                        .ListenWeakImpl(entries =>
                        {
                            foreach (Entry e in entries)
                            {
                                e.Item.Cancellation.Cancel();
                            }
                        });
            }

            if (this.cancelMatching != null)
            {
                this.cancelMatchingListener =
                    this.cancelMatching
                        .SnapshotImpl(c: trackedCell, f: (toCancel, entries) => (ToCancel: toCancel, Entries: entries))
                        .ListenWeakImpl(pair =>
                        {
                            if (pair.ToCancel.Count == 0)
                            {
                                return;
                            }

                            // Compared directly against each tracked Entry's original TInput value
                            // (Entry.Value) using the default equality comparer for TInput —
                            // cancelMatching is expressed in terms of TInput, not TStrategyInput, so no
                            // conversion is needed here.
                            HashSet<TInput> targets = new(pair.ToCancel);

                            foreach (Entry e in pair.Entries)
                            {
                                if (targets.Contains(e.Item.Value))
                                {
                                    e.Item.Cancellation.Cancel();
                                }
                            }
                        });
            }

            // Always wired, regardless of whether the caller passed their own cancelAll — this
            // is what a Dispose() with cancelOnDispose: true fires into. ListenWeak here too, for
            // uniformity, though it's less load-bearing: disposeCancelTrigger is our own field,
            // so this pair was always part of the same reference graph as this execution manager
            // either way, and .NET's GC collects unreachable cycles regardless of Listen vs
            // ListenWeak.
            this.disposeCancelListener =
                this.disposeCancelTrigger
                    .SnapshotImpl(c: trackedCell, f: (_, entries) => entries)
                    .ListenWeakImpl(entries =>
                    {
                        foreach (Entry e in entries)
                        {
                            e.Item.Cancellation.Cancel();
                        }
                    });

            Cell<bool> isRunning =
                trackedCell.MapImpl(entries =>
                    Array.Exists(array: entries, match: e => e.Status == AsyncItemStatus.Running));

            Cell<IReadOnlyList<AsyncItem<TInput>>> items =
                trackedCell.MapImpl<IReadOnlyList<AsyncItem<TInput>>>(entries =>
                    Array.ConvertAll(
                        array: entries,
                        converter: e => new AsyncItem<TInput>(value: e.Item.Value, status: e.Status)));

            return new AsyncMapStatus<TInput>(isRunning: isRunning, items: items, dispose: this.Dispose);
        }

        // The one deliberate fire-and-forget boundary in this class. StartOperation's own
        // try/catch/finally is exhaustive, so the Task it returns is expected to always
        // complete successfully — nothing here needs its result. If it somehow faults anyway,
        // that's a bug in this class rather than something worth swallowing, so it's re-thrown
        // (with its original stack trace preserved) instead of silently becoming an unobserved
        // task exception.
        private static void FireAndForget(Task task) =>
            task.ContinueWith(
                continuationAction: t => ExceptionDispatchInfo.Capture(t.Exception!.GetBaseException()).Throw(),
                cancellationToken: CancellationToken.None,
                continuationOptions: TaskContinuationOptions.OnlyOnFaulted |
                                     TaskContinuationOptions.ExecuteSynchronously,
                scheduler: TaskScheduler.Default);

        // Used both as mutations' own same-transaction coalescing function (needed because
        // Complete can recurse — see Complete's remarks — causing more than one Send() to this
        // sink within a single transaction) and as the combiner for starts.Merge(mutations, ...)
        // in Attach, which handles the separate case of starts and mutations both firing in the
        // same transaction (e.g. a strategy's Admit promoting an already-canceled older item
        // it's holding onto, alongside the brand-new one it's admitting).
        private static Mutation CombineMutations(Mutation a, Mutation b) =>
            new(
                remove: Concat(a: a.Remove, b: b.Remove),
                promote: Concat(a: a.Promote, b: b.Promote),
                add: Concat(a: a.Add, b: b.Add));

        private static Entry[] Apply(Entry[] list, Mutation mutation)
        {
            if (mutation.Remove.Length > 0)
            {
                Entry[] kept = new Entry[list.Length];
                int count = 0;

                foreach (Entry e in list)
                {
                    bool remove = mutation.Remove.Any(itemToRemove => e.Item.Id == itemToRemove);

                    if (!remove)
                    {
                        kept[count++] = e;
                    }
                }

                if (count != list.Length)
                {
                    Array.Resize(array: ref kept, newSize: count);
                    list = kept;
                }
            }

            if (mutation.Add.Length > 0)
            {
                list = Concat(a: list, b: mutation.Add);
            }

            if (mutation.Promote.Length > 0)
            {
                Entry[]? updated = null;

                foreach (Guid idToPromote in mutation.Promote)
                {
                    int idx =
                        Array.FindIndex(
                            array: list,
                            match: e => e.Item.Id == idToPromote);

                    if (idx >= 0 && list[idx].Status != AsyncItemStatus.Running)
                    {
                        if (updated == null)
                        {
                            updated = new Entry[list.Length];
                            Array.Copy(sourceArray: list, destinationArray: updated, length: list.Length);
                        }

                        updated[idx] = updated[idx].WithStatus(AsyncItemStatus.Running);
                    }
                }

                if (updated != null)
                {
                    list = updated;
                }
            }

            return list;
        }

        private static T[] Concat<T>(T[] a, T[] b)
        {
            if (a.Length == 0)
            {
                return b;
            }

            if (b.Length == 0)
            {
                return a;
            }

            T[] result = new T[a.Length + b.Length];
            Array.Copy(sourceArray: a, destinationArray: result, length: a.Length);

            Array.Copy(
                sourceArray: b,
                sourceIndex: 0,
                destinationArray: result,
                destinationIndex: a.Length,
                length: b.Length);

            return result;
        }

        /// <summary>
        ///     Stops this pipeline — see <see cref="AsyncMapStatus{TInput}.Dispose" /> for the full
        ///     contract. Whether to cancel what's currently tracked was fixed at Attach time via
        ///     cancelOnDispose, not passed in here — IDisposable.Dispose() is deliberately the only
        ///     public way to dispose. Guarded by <see cref="disposeState" /> so it only ever runs
        ///     once, since this can be called from any thread with no Sodium transaction open yet.
        /// </summary>
        private void Dispose()
        {
            if (Interlocked.CompareExchange(location1: ref this.disposeState, value: 1, comparand: 0) != 0)
            {
                return;
            }

            TransactionInternal.RunImpl(() =>
            {
                this.disposed = true;

                if (this.cancelOnDispose)
                {
                    this.disposeCancelTrigger.SendImpl(Unit.Value);
                }

                return Unit.Value;
            });

            this.cancelAllListener?.Unlisten();
            this.cancelMatchingListener?.Unlisten();
            this.disposeCancelListener?.Unlisten();
        }

        private void PromoteAndLaunch(
            AsyncToStart<TStrategyInput> toStart,
            TInput value,
            Cell<Dictionary<Guid, Entry>> entryByIdCell)
        {
            if (toStart.Item.Cancellation.IsCancellationRequested)
            {
                // Canceled while it was still queued — finish immediately without ever
                // invoking the operation. This still goes through the normal completion path,
                // so a strategy like the built-in Queue naturally moves on to whatever's next.
                this.Complete(
                    item: toStart.Item,
                    strategyOutcome: AsyncOutcome<TStrategyResult>.Canceled(),
                    outcome: AsyncOutcome<TResult>.Canceled(),
                    entryByIdCell: entryByIdCell);

                return;
            }

            // Only the actual invocation of the operation is deferred — the entry was already
            // marked Running synchronously, in the transaction that decided to promote it. Post
            // just avoids kicking off real async work (a side effect) while still inside the
            // transaction that's processing the triggering event. StartOperation is an ordinary
            // async Task method; FireAndForget is the one place its Task is deliberately not
            // awaited, made explicit rather than folding the discard into an async void method.
            TransactionInternal.PostImpl(() =>
                FireAndForget(this.StartOperation(toStart: toStart, value: value, entryByIdCell: entryByIdCell)));
        }

        private async Task StartOperation(
            AsyncToStart<TStrategyInput> toStart,
            TInput value,
            Cell<Dictionary<Guid, Entry>> entryByIdCell)
        {
            // The operation observes the strategy's own token (if any — e.g. an external
            // timeout) AND this item's own cancellation, linked together.
            CancellationTokenSource linked =
                CancellationTokenSource.CreateLinkedTokenSource(
                    token1: toStart.StrategyToken,
                    token2: toStart.Item.Cancellation.Token);

            try
            {
                // Calling and awaiting the operation directly, inline, on this thread: it only
                // ever moves to another thread if and when it needs to (e.g. at its own first
                // await) — we never force a thread-pool hop just to get it started. If it's
                // already finished by the time we reach the await, await resumes synchronously
                // too, so there's no continuation hop on that path either. `value` is the original
                // TInput this item was admitted with, threaded through from PromoteAndLaunch — see
                // the class remarks — not anything recovered from TStrategyInput.
                TResult result =
                    await this.operation(arg1: value, arg2: linked.Token)
                        .ConfigureAwait(false);

                this.Complete(
                    item: toStart.Item,
                    strategyOutcome: AsyncOutcome<TStrategyResult>.Succeeded(this.resultConverter(result)),
                    outcome: AsyncOutcome<TResult>.Succeeded(result),
                    entryByIdCell: entryByIdCell);
            }
            catch (OperationCanceledException oce) when (oce.CancellationToken == linked.Token)
            {
                this.Complete(
                    item: toStart.Item,
                    strategyOutcome: AsyncOutcome<TStrategyResult>.Canceled(),
                    outcome: AsyncOutcome<TResult>.Canceled(),
                    entryByIdCell: entryByIdCell);
            }
            catch (Exception ex)
            {
                this.Complete(
                    item: toStart.Item,
                    strategyOutcome: AsyncOutcome<TStrategyResult>.Failed(ex),
                    outcome: AsyncOutcome<TResult>.Failed(ex),
                    entryByIdCell: entryByIdCell);
            }
            finally
            {
                linked.Dispose();
            }
        }

        /// <summary>
        ///     The single place that turns a finished (or never-started, if canceled while queued)
        ///     item into effects: asks the strategy what to do, then — as one atomic Sodium
        ///     transaction — publishes the outcome if asked, removes this item's entry, promotes
        ///     whatever the strategy chose to start next, and retires this item's
        ///     CancellationTokenSource. Retiring the entry and disposing its CancellationTokenSource
        ///     in the same transaction means a cancellation stream's Snapshot can never observe a
        ///     stale entry: it either sees it (still cancellable) from before this transaction, or
        ///     doesn't see it at all, from after. This can recurse (e.g. draining several
        ///     already-canceled queued items in one go via PromoteAndLaunch's short-circuit) —
        ///     Sodium's transactions nest safely, but a very long queued-and-all-canceled backlog
        ///     would recurse proportionally deep.
        /// </summary>
        private void Complete(
            AsyncQueuedItem<TStrategyInput> item,
            AsyncOutcome<TStrategyResult> strategyOutcome,
            AsyncOutcome<TResult> outcome,
            Cell<Dictionary<Guid, Entry>> entryByIdCell) =>
            TransactionInternal.RunImpl(() =>
            {
                Dictionary<Guid, Entry> entryById = entryByIdCell.SampleImpl();

                AsyncStrategyResult<TStrategyInput> decision =
                    this.stateManager.OnCompleted(item: item, outcome: strategyOutcome);

                if (decision.Publish)
                {
                    outcome.MatchVoid(
                        // `result` is the actual TResult StartOperation produced, threaded through as
                        // its own parameter — not recovered from outcome.Value, which holds the
                        // converted TStrategyResult instead. See the class remarks.
                        onSucceeded: this.results.SendImpl,
                        onFailed: this.errors.SendImpl,
                        onCanceled: null);
                }

                Guid[] promote = new Guid[decision.Next.Count];
                TInput[] values = new TInput[decision.Next.Count];

                for (int i = 0; i < decision.Next.Count; i++)
                {
                    if (entryById.ContainsKey(decision.Next[i].Item.Id))
                    {
                        values[i] = entryById[decision.Next[i].Item.Id].Value;
                    }
                    else
                    {
                        throw new Exception("Could not find item to start.");
                    }

                    promote[i] = decision.Next[i].Item.Id;
                }

                this.mutations.SendImpl(
                    new Mutation(
                        remove: new[] { item.Id },
                        promote: promote,
                        add: Array.Empty<Entry>()));

                item.Cancellation.Dispose();

                for (int i = 0; i < decision.Next.Count; i++)
                {
                    this.PromoteAndLaunch(
                        toStart: decision.Next[i],
                        value: values[i],
                        entryByIdCell: entryByIdCell);
                }

                return Unit.Value;
            });

        // ---- Tracked-items bookkeeping (private to the execution engine — invisible to strategies) ----

        private sealed class Entry
        {
            public Entry(TInput value, AsyncQueuedItem<TInput> item, AsyncItemStatus status)
            {
                this.Value = value;
                this.Item = item;
                this.Status = status;
            }

            public TInput Value { get; }

            public AsyncQueuedItem<TInput> Item { get; }

            public AsyncItemStatus Status { get; }

            public Entry WithStatus(AsyncItemStatus status) => new(value: this.Value, item: this.Item, status: status);
        }

        private sealed class Mutation
        {
            public Mutation(
                Guid[] remove,
                Guid[] promote,
                Entry[] add)
            {
                this.Remove = remove;
                this.Promote = promote;
                this.Add = add;
            }

            public Guid[] Remove { get; }

            public Guid[] Promote { get; }

            public Entry[] Add { get; }
        }
    }
}

/*
Usage:

    StreamSink<string> requests = new StreamSink<string>();
    StreamSink<SearchResults> results = new StreamSink<SearchResults>();
    StreamSink<Exception> errors = new StreamSink<Exception>();
    StreamSink<Unit> cancelAll = new StreamSink<Unit>();                            // e.g. a "Cancel" button
    StreamSink<IReadOnlyCollection<string>> cancelMatching =
        new StreamSink<IReadOnlyCollection<string>>();                              // e.g. per-row cancel buttons

    // The strategy instance is stateless and reusable — it's fine to keep this one around and
    // pass it to multiple MapAsync calls, even concurrently; each call gets its own independent
    // scheduling state under the hood.
    //
    // cancelOnDispose (true by default) is fixed here, at setup — not something you choose
    // later at Dispose() time.
    using AsyncMapStatus<string> status = requests.MapAsync(
        results: results,
        errors: errors,
        operation: async (query, ct) => await searchService.SearchAsync(query, ct),
        strategy: AsyncConcurrencyStrategy<string, SearchResults>.Queue(),
        cancelAll: cancelAll,
        cancelMatching: cancelMatching,
        cancelOnDispose: true);

    Cell<bool> isSearching = status.IsRunning;                  // true only while something is actually executing
    Cell<IReadOnlyList<AsyncItem<string>>> queueView = status.Items; // both Queued and Running entries, for e.g. a queue-position display

    // cancelAll.Send(Unit.Value) cancels everything, queued or running.
    // cancelMatching.Send(new[] { "some query" }) cancels just that one, wherever it currently sits.

    // Tearing the whole thing down (e.g. when the owning view is closed) — this is the ONLY way
    // to dispose; there's no overload taking a bool here, because that choice was already made
    // above via cancelOnDispose. Disposing always, unconditionally, stops any further requests
    // from ever being admitted; whether it also cancels what's already tracked follows whatever
    // cancelOnDispose was set to at setup.
    status.Dispose();

Custom concurrency logic:
    Subclass AsyncConcurrencyStrategy<TInput, TResult, TState> and implement CreateState/Admit/OnCompleted
    as pure reporting — they have no access to the result/error sinks and don't start Tasks
    themselves; they just describe what should happen and let the execution engine carry it out.
    CreateState is called once per MapAsync call, so TState is where all of a strategy's mutable
    bookkeeping lives instead of on the strategy instance itself — that's what makes a single
    strategy instance safe to reuse across multiple MapAsync calls without their scheduling state
    bleeding into each other. The one thing you can do imperatively is item.Cancel(), to cancel an
    item you're managing (queued or running); it still completes normally through OnCompleted as
    Outcome.Canceled, so you can chain from there. Every value you don't immediately start stays
    tracked as Queued automatically; hold onto its AsyncQueuedItem (not just its Value) in TState if
    you intend to promote it later or recognize it again in OnCompleted — AsyncQueuedItem is a
    sealed class you can't construct, and identity is reference identity, so ReferenceEquals (as the
    built-in SwitchLatest strategy does) is enough for "is this still current?" checks, with no
    separate handle needed. A strategy that neither starts nor remembers an incoming value leaves it
    permanently Queued — if you want a "reject outright" behavior, promote it immediately and have
    your own logic complete it right away instead.

Sharing one strategy across several TInput/TResult types:
    AsyncConcurrencyStrategy<TInput, TResult, TState> doesn't have to be written against the exact
    TInput/TResult of any one MapAsync call. MapAsync itself is
    MapAsync<TInput, TResult, TStrategyInput, TStrategyResult, TState>, with
    `where TInput : TStrategyInput` and `where TResult : TStrategyResult` — so a strategy written
    against a common base or interface (e.g. AsyncConcurrencyStrategy<IRequest, IResponse, TState>)
    can be handed to MapAsync calls over several different, more specific Stream<TInput> /
    StreamSink<TResult> pairs, one shared instance governing all of them together (e.g. one Queue
    strategy serializing every kind of IRequest across multiple streams). TStrategyInput/
    TStrategyResult are always inferred from the strategy argument's type — for the common case
    where the strategy is written against the call's exact types, they're simply equal to
    TInput/TResult and nothing needs to be specified explicitly.
*/