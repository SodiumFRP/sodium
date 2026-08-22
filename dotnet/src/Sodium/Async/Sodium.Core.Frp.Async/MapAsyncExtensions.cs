using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.ExceptionServices;
using System.Threading;
using System.Threading.Tasks;
using Sodium.Functional;

namespace Sodium.Frp.Async
{
    /// <summary>Whether a tracked item is waiting for a slot or actually executing.</summary>
    public enum AsyncItemStatus
    {
        Queued,
        Running
    }

    /// <summary>An input value being tracked by a MapAsync pipeline, and its current status.</summary>
    public readonly struct AsyncItem<TInput>
    {
        public AsyncItem(TInput value, AsyncItemStatus status)
        {
            this.Value = value;
            this.Status = status;
        }

        public TInput Value { get; }

        public AsyncItemStatus Status { get; }
    }

    /// <summary>
    ///     The observable status of a MapAsync pipeline: whether anything is actively running, and
    ///     every input value currently tracked (queued or running) along with its status. Also, the
    ///     sole handle for tearing the pipeline down — see <see cref="Dispose" />.
    /// </summary>
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
    ///     Extension methods that bridge an impure asynchronous operation into the FRP world:
    ///     listen on a Stream&lt;TInput&gt;, run an async operation per firing, push the result into a
    ///     StreamSink&lt;TResult&gt;, and expose what's queued/running — optionally wired up to streams
    ///     that trigger cancellation of queued or running work alike. The returned
    ///     <see cref="AsyncMapStatus{TInput}" /> is IDisposable; disposing it is how you tear the whole
    ///     pipeline down.
    /// </summary>
    public static class AsyncStreamExtensions
    {
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
        ///     How overlapping requests are handled. Defaults to
        ///     <see cref="AsyncConcurrencyStrategy{TInput,TResult}.Queue" />.
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
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />, or
        ///     <paramref name="operation" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategy<TInput, TResult>? strategy = null,
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

            strategy ??= AsyncConcurrencyStrategy<TInput, TResult>.Queue();

            return strategy.Attach(
                source: source,
                results: results,
                errors: errors,
                operation: operation,
                cancelAll: cancelAll,
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);
        }
    }

    /// <summary>
    ///     Strategy for how a stream of async requests is scheduled. All the mechanics — starting
    ///     operations, catching exceptions, routing results/errors, tracking queued/running items,
    ///     wiring up external cancellation (including cancelling something before it ever starts),
    ///     transaction boundaries — live in this base class. A concrete strategy (built-in or custom)
    ///     implements only two questions, both answered as plain data: <see cref="Admit" /> ("given
    ///     this newly-tracked value, what do I start now?") and <see cref="OnCompleted" /> ("given
    ///     this outcome, what do I start next, and should it be published?"). Neither can touch the
    ///     result/error sinks or a Task directly — they're private to this class, so publishing
    ///     happens only via StrategyResult.Publish. The one imperative affordance is
    ///     <see cref="Cancel" />, for cancelling an item the strategy is managing — it doesn't
    ///     publish anything or start anything, it just routes into the same cancellation path an
    ///     external cancelAll stream uses, so the item still completes through
    ///     <see cref="OnCompleted" /> like any other. A completed item's identity is just
    ///     the same <see cref="QueuedItem" /> the strategy already saw in <see cref="Admit" /> — there's
    ///     no separate handle to plumb through; hold onto the QueuedItem itself if you need to
    ///     recognize it again later.
    /// </summary>
    public abstract class AsyncConcurrencyStrategy<TInput, TResult>
    {
        private static readonly Mutation NoMutation =
            new(
                remove: Array.Empty<QueuedItem>(),
                promote: Array.Empty<QueuedItem>(),
                add: Array.Empty<Entry>());

        // Carries edits (add/promote/remove) to the tracked-items list. Always sent from either
        // Map's transform (not a registered Listen() callback — see Attach) or from well outside
        // any Sodium callback (background-thread continuations) — send() is legal in both cases.
        private readonly StreamSink<Mutation> mutations =
            StreamInternal.CreateSinkImpl<Mutation>(CombineMutations);

        // Always sent inside a transaction (from Dispose) — reuses the exact same
        // Snapshot(tracked)+Cancel() pattern as a user-supplied cancelAll stream. Wired up
        // unconditionally in Attach, regardless of whether the caller passed their own cancelAll.
        private readonly StreamSink<Unit> disposeCancelTrigger = StreamInternal.CreateSinkImpl<Unit>();

        // No lock guards Admit/OnCompleted's mutable state (queues, active items, etc.), and
        // none is needed: both are only ever called from inside a Sodium transaction (Attach's
        // Map, and Complete's Transaction.RunVoid), and Sodium serializes ALL transactions
        // process-wide behind one global lock — at most one is ever in progress anywhere, on
        // any thread. This relies specifically on that "one transaction at a time" guarantee;
        // if you're on a Sodium implementation that doesn't provide it, this state would need
        // its own lock again.
        private Func<TInput, CancellationToken, Task<TResult>> operation = null!;

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
        // why). As long as the strategy itself is reachable, these fields keep the
        // subscriptions alive; once nothing references the strategy, these go with it and the
        // subscriptions lapse on their own. Dispose additionally Unlistens them explicitly, for
        // immediate/deterministic detachment rather than waiting on GC.
        private IListener? cancelAllListener;

        private IListener? cancelMatchingListener;

        private IListener? disposeCancelListener;

        // Deliberately private, not protected: a strategy reports what should be published via
        // StrategyResult.Publish and the base class does the sending in Complete, where it's
        // batched into the same transaction as the entry removal and any follow-on launches.
        // Handing a derived strategy direct access would let it bypass all of that, so the
        // "strategies never publish directly" rule is enforced here rather than merely documented.
        private StreamSink<TResult> results = null!;

        private StreamSink<Exception> errors = null!;

        // Fixed once, at setup, and read only by Dispose — see MapAsync's cancelOnDispose
        // parameter. Not mutated after Attach, so no transaction/Interlocked protection needed.
        private bool cancelOnDispose;

        /// <summary>Every firing starts its own operation immediately; results arrive in completion order.</summary>
        public static AsyncConcurrencyStrategy<TInput, TResult> Parallel() => new ParallelStrategy();

        /// <summary>At most one operation runs at a time; later firings queue and run in order.</summary>
        public static AsyncConcurrencyStrategy<TInput, TResult> Queue() => new QueueStrategy();

        /// <summary>A new firing cancels whatever is currently in flight and takes its place.</summary>
        public static AsyncConcurrencyStrategy<TInput, TResult> SwitchLatest() => new SwitchLatestStrategy();

        internal AsyncMapStatus<TInput> Attach(
            Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            Stream<Unit>? cancelAll,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching,
            bool cancelOnDispose)
        {
            this.results = results;
            this.errors = errors;
            this.operation = operation;
            this.cancelOnDispose = cancelOnDispose;

            // Map runs as ordinary transaction-processing code, not a registered Listen()
            // callback, so it isn't subject to Sodium's "no send() inside a callback"
            // restriction — and it fires in the SAME transaction as the source. Every admitted
            // value is tracked from this moment on: it's added as Queued, then immediately
            // promoted to Running for whichever ToStart(s) Admit returns (normally just the
            // value itself, for the built-in strategies). Once disposed, this is a permanent
            // no-op — Admit is never called again, so nothing new is ever queued or started.
            Stream<Mutation> starts =
                source.MapImpl(value =>
                {
                    if (this.disposed)
                    {
                        return NoMutation;
                    }

                    CancellationTokenSource cancellation = new();
                    QueuedItem incoming = new(value: value, cancellation: cancellation);
                    Entry newEntry = new(item: incoming, status: AsyncItemStatus.Queued);

                    IReadOnlyList<ToStart> toStart = this.Admit(incoming);
                    QueuedItem[] promote = new QueuedItem[toStart.Count];

                    for (int i = 0; i < toStart.Count; i++)
                    {
                        promote[i] = toStart[i].Item;
                        this.PromoteAndLaunch(toStart[i]);
                    }

                    return new Mutation(
                        remove: Array.Empty<QueuedItem>(),
                        promote: promote,
                        add: new[] { newEntry });
                });

            Cell<Entry[]> tracked =
                starts
                    .MergeImpl(s: this.mutations, f: CombineMutations)
                    .AccumImpl(
                        initialState: Array.Empty<Entry>(),
                        f: (mutation, list) =>
                            Apply(list: list, mutation: mutation));

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
            // (this strategy, the result/error sinks, everything reachable from `tracked`) alive
            // forever, whether or not the caller still references IsRunning/Items — Dispose
            // would become the ONLY way to ever release it. With ListenWeak, the subscription
            // only survives as long as something else keeps the strategy reachable (normally
            // the caller holding onto IsRunning/Items); once nothing does, the whole thing
            // becomes collectible together, and this callback simply stops firing. This is a
            // safety net for a forgotten Dispose, not a replacement for calling it: GC timing
            // is non-deterministic, and it does nothing for a Task that's already running —
            // that keeps executing to completion on its own regardless of whether anything is
            // still listening for cancellation.
            if (cancelAll != null)
            {
                this.cancelAllListener =
                    cancelAll
                        .SnapshotImpl(c: tracked, f: (_, entries) => entries)
                        .ListenWeakImpl(entries =>
                        {
                            foreach (Entry e in entries)
                            {
                                e.Item.Cancellation.Cancel();
                            }
                        });
            }

            if (cancelMatching != null)
            {
                this.cancelMatchingListener =
                    cancelMatching
                        .SnapshotImpl(c: tracked, f: (toCancel, entries) => (ToCancel: toCancel, Entries: entries))
                        .ListenWeakImpl(pair =>
                        {
                            if (pair.ToCancel.Count == 0)
                            {
                                return;
                            }

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
            // so this pair was always part of the same reference graph as the strategy itself
            // either way, and .NET's GC collects unreachable cycles regardless of Listen vs
            // ListenWeak.
            this.disposeCancelListener =
                this.disposeCancelTrigger
                    .SnapshotImpl(c: tracked, f: (_, entries) => entries)
                    .ListenWeakImpl(entries =>
                    {
                        foreach (Entry e in entries)
                        {
                            e.Item.Cancellation.Cancel();
                        }
                    });

            Cell<bool> isRunning =
                tracked.MapImpl(entries =>
                    Array.Exists(array: entries, match: e => e.Status == AsyncItemStatus.Running));

            Cell<IReadOnlyList<AsyncItem<TInput>>> items =
                tracked.MapImpl(entries =>
                    (IReadOnlyList<AsyncItem<TInput>>)Array.ConvertAll(
                        array: entries,
                        converter: e => new AsyncItem<TInput>(value: e.Item.Value, status: e.Status)));

            return new AsyncMapStatus<TInput>(isRunning: isRunning, items: items, dispose: this.Dispose);
        }

        /// <summary>
        ///     Given a newly admitted value (already tracked as Queued — see <see cref="QueuedItem" />),
        ///     which item(s) should start right now? Always called from inside a Sodium transaction
        ///     (see the class remarks on why that makes this safe without an explicit lock). Return
        ///     a <see cref="ToStart" /> wrapping <paramref name="incoming" /> to start it immediately;
        ///     omitting it leaves it Queued. If you leave it Queued, hold onto <paramref name="incoming" />
        ///     itself (not just its Value) in your own state if you ever want to start it later —
        ///     that's what preserves its identity and cancellation across the wait, and it's the same
        ///     value you'll be handed back in <see cref="OnCompleted" />.
        /// </summary>
        protected abstract IReadOnlyList<ToStart> Admit(QueuedItem incoming);

        /// <summary>
        ///     Given the outcome of a finished item, should it be published, and which
        ///     previously-tracked item(s) — if any — should start right now as a consequence (e.g. the next
        ///     queued item)? Always called from inside a Sodium transaction, same as <see cref="Admit" />.
        ///     Each <see cref="ToStart" /> returned here must wrap a <see cref="QueuedItem" /> you
        ///     already hold from an earlier <see cref="Admit" /> call — there's no way to introduce a
        ///     value that wasn't previously admitted. <paramref name="item" /> is the exact same
        ///     QueuedItem this strategy was handed for this value back in <see cref="Admit" /> — the
        ///     same instance, so ReferenceEquals against anything you stashed away answers "is this
        ///     still the current run?". Canceled outcomes are never published regardless of
        ///     what's returned here — cancellation (external, a strategy superseding its own prior run,
        ///     or a queued item canceled before its turn came) is always treated as an expected,
        ///     silent completion.
        /// </summary>
        protected abstract StrategyResult OnCompleted(QueuedItem item, Outcome outcome);

        /// <summary>
        ///     Cancels a specific tracked item — the same mechanism a cancelAll/cancelMatching
        ///     stream uses, available to a strategy for its own scheduling decisions (e.g.
        ///     SwitchLatest superseding its previous run). Works whether <paramref name="item" /> is
        ///     still Queued or already Running: a queued item is simply never started when its turn
        ///     comes, and a running one only actually stops if its operation observes the
        ///     CancellationToken it was given. Either way the item completes normally, as
        ///     <see cref="Outcome.Cancelled" />, so <see cref="OnCompleted" /> still runs for it and
        ///     can chain to whatever's next.
        ///     Safe to call on an item that has already completed, already been canceled, or is
        ///     mid-completion — those are no-ops rather than errors, so a strategy holding a stale
        ///     reference doesn't have to track precisely when an item stopped being cancellable.
        /// </summary>
        protected void Cancel(QueuedItem item)
        {
            if (item is null)
            {
                throw new ArgumentNullException(nameof(item));
            }

            // Complete disposes the item's CancellationTokenSource once it's done, so a stale
            // reference can land here after disposal. Cancel() on a disposed CTS throws, and
            // "already finished" is exactly the case a strategy shouldn't have to guard against,
            // so swallow just that.
            try
            {
                item.Cancellation.Cancel();
            }
            catch (ObjectDisposedException)
            {
            }
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
                    bool remove =
                        mutation.Remove.Any(itemToRemove => ReferenceEquals(objA: e.Item, objB: itemToRemove));

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

                foreach (QueuedItem itemToPromote in mutation.Promote)
                {
                    int idx =
                        Array.FindIndex(
                            array: list,
                            match: e => ReferenceEquals(objA: e.Item, objB: itemToPromote));

                    if (idx >= 0 && list[idx].Status != AsyncItemStatus.Running)
                    {
                        updated ??= (Entry[])list.Clone();
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

        private void PromoteAndLaunch(ToStart toStart)
        {
            if (toStart.Item.Cancellation.IsCancellationRequested)
            {
                // Canceled while it was still queued — finish immediately without ever
                // invoking the operation. This still goes through the normal completion path,
                // so a strategy like QueueStrategy naturally moves on to whatever's next.
                this.Complete(item: toStart.Item, outcome: Outcome.Cancelled());

                return;
            }

            // Only the actual invocation of the operation is deferred — the entry was already
            // marked Running synchronously, in the transaction that decided to promote it. Post
            // just avoids kicking off real async work (a side effect) while still inside the
            // transaction that's processing the triggering event. StartOperation is an ordinary
            // async Task method; FireAndForget is the one place its Task is deliberately not
            // awaited, made explicit rather than folding the discard into an async void method.
            TransactionInternal.PostImpl(() =>
                FireAndForget(this.StartOperation(toStart)));
        }

        private async Task StartOperation(ToStart toStart)
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
                // too, so there's no continuation hop on that path either.
                TResult result =
                    await this.operation(arg1: toStart.Item.Value, arg2: linked.Token).ConfigureAwait(false);

                this.Complete(item: toStart.Item, outcome: Outcome.Succeeded(result));
            }
            catch (OperationCanceledException oce) when (oce.CancellationToken == linked.Token)
            {
                this.Complete(item: toStart.Item, outcome: Outcome.Cancelled());
            }
            catch (Exception ex)
            {
                this.Complete(item: toStart.Item, outcome: Outcome.Failed(ex));
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
        private void Complete(QueuedItem item, Outcome outcome) =>
            TransactionInternal.RunImpl(() =>
            {
                StrategyResult decision = this.OnCompleted(item: item, outcome: outcome);

                if (decision.Publish && outcome.Kind != OutcomeKind.Cancelled)
                {
                    if (outcome.Kind == OutcomeKind.Succeeded)
                    {
                        this.results.SendImpl(outcome.Value!);
                    }
                    else
                    {
                        this.errors.SendImpl(outcome.Error!);
                    }
                }

                QueuedItem[] promote = new QueuedItem[decision.Next.Count];

                for (int i = 0; i < decision.Next.Count; i++)
                {
                    promote[i] = decision.Next[i].Item;
                }

                this.mutations.SendImpl(
                    new Mutation(
                        remove: new[] { item },
                        promote: promote,
                        add: Array.Empty<Entry>()));

                item.Cancellation.Dispose();

                foreach (ToStart next in decision.Next)
                {
                    this.PromoteAndLaunch(next);
                }

                return Unit.Value;
            });

        /// <summary>
        ///     A value the base class is tracking, from the moment it's admitted until it's
        ///     promoted, completed, or canceled — and the single object that identifies it
        ///     throughout, in both <see cref="ToStart" /> and <see cref="OnCompleted" />. Opaque by
        ///     design: a strategy can hold onto one (typically to promote later, or to recognize it
        ///     again on completion) and read its Value, but can't construct one — the constructor is
        ///     internal and the class is sealed, so every QueuedItem in existence originates from an
        ///     <see cref="Admit" /> call. Identity is reference identity: the instance itself IS the
        ///     id, so comparing two with ReferenceEquals (or ==) tells you whether they're the same
        ///     admitted value.
        /// </summary>
        protected sealed class QueuedItem
        {
            internal QueuedItem(TInput value, CancellationTokenSource cancellation)
            {
                this.Value = value;
                this.Cancellation = cancellation;
            }

            public TInput Value { get; }

            internal CancellationTokenSource Cancellation { get; }
        }

        /// <summary>A previously-tracked item to start (or promote from Queued to Running) right now.</summary>
        protected readonly struct ToStart
        {
            public ToStart(QueuedItem item, CancellationToken strategyToken = default)
            {
                this.Item = item ?? throw new ArgumentNullException(nameof(item));
                this.StrategyToken = strategyToken;
            }

            public QueuedItem Item { get; }

            /// <summary>
            ///     Optional extra cancellation source to link into this run, on top of the item's
            ///     own. Not needed to cancel an item the strategy is managing — use
            ///     <see cref="AsyncConcurrencyStrategy{TInput,TResult}.Cancel" /> for that. This is for tying a
            ///     run to something external instead: a per-request timeout, an ambient operation
            ///     token, a shared token covering a batch of work. Leave it defaulted otherwise.
            /// </summary>
            public CancellationToken StrategyToken { get; }
        }

        /// <summary>The three ways an item can finish.</summary>
        protected enum OutcomeKind
        {
            Succeeded,
            Failed,
            Cancelled
        }

        /// <summary>How an item finished.</summary>
        protected readonly struct Outcome
        {
            private Outcome(OutcomeKind kind, TResult? value, Exception? error)
            {
                this.Kind = kind;
                this.Value = value;
                this.Error = error;
            }

            public OutcomeKind Kind { get; }

            public TResult? Value { get; }

            public Exception? Error { get; }

            public static Outcome Succeeded(TResult value) =>
                new(kind: OutcomeKind.Succeeded, value: value, error: null);

            public static Outcome Failed(Exception error) =>
                new(kind: OutcomeKind.Failed, value: default, error: error);

            public static Outcome Cancelled() => new(kind: OutcomeKind.Cancelled, value: default, error: null);
        }

        /// <summary>
        ///     A strategy's answer: whether to publish the just-finished outcome, and what to start next.
        /// </summary>
        protected readonly struct StrategyResult
        {
            public static readonly IReadOnlyList<ToStart> None = Array.Empty<ToStart>();

            public StrategyResult(bool publish, IReadOnlyList<ToStart> next)
            {
                this.Publish = publish;
                this.Next = next;
            }

            public bool Publish { get; }

            public IReadOnlyList<ToStart> Next { get; }
        }

        // ---- Tracked-items bookkeeping (private — invisible to strategies) ----

        private readonly struct Entry
        {
            public Entry(QueuedItem item, AsyncItemStatus status)
            {
                this.Item = item;
                this.Status = status;
            }

            public QueuedItem Item { get; }

            public AsyncItemStatus Status { get; }

            public Entry WithStatus(AsyncItemStatus status) => new(item: this.Item, status: status);
        }

        private readonly struct Mutation
        {
            public Mutation(QueuedItem[] remove, QueuedItem[] promote, Entry[] add)
            {
                this.Remove = remove;
                this.Promote = promote;
                this.Add = add;
            }

            public QueuedItem[] Remove { get; }

            public QueuedItem[] Promote { get; }

            public Entry[] Add { get; }
        }

        // ---- Built-in strategies ----

        private sealed class ParallelStrategy
            : AsyncConcurrencyStrategy<TInput, TResult>
        {
            protected override IReadOnlyList<ToStart> Admit(QueuedItem incoming) => new[] { new ToStart(incoming) };

            protected override StrategyResult OnCompleted(QueuedItem item, Outcome outcome) =>
                new(publish: true, next: StrategyResult.None);
        }

        private sealed class QueueStrategy
            : AsyncConcurrencyStrategy<TInput, TResult>
        {
            private readonly Queue<QueuedItem> pending = new();

            private bool busy;

            protected override IReadOnlyList<ToStart> Admit(QueuedItem incoming)
            {
                if (this.busy)
                {
                    // Stays visible as Queued; still cancellable while it waits.
                    this.pending.Enqueue(incoming);

                    return StrategyResult.None;
                }

                this.busy = true;

                return new[] { new ToStart(incoming) };
            }

            protected override StrategyResult OnCompleted(QueuedItem item, Outcome outcome)
            {
                if (this.pending.Count > 0)
                {
                    QueuedItem next = this.pending.Dequeue();

                    // If `next` was canceled while it sat here, PromoteAndLaunch will notice
                    // and short-circuit straight to Outcome.Cancelled(), which calls back into
                    // OnCompleted and naturally dequeues whatever comes after it.
                    return new StrategyResult(publish: true, next: new[] { new ToStart(next) });
                }

                this.busy = false;

                return new StrategyResult(publish: true, next: StrategyResult.None);
            }
        }

        private sealed class SwitchLatestStrategy
            : AsyncConcurrencyStrategy<TInput, TResult>
        {
            private QueuedItem? active;

            protected override IReadOnlyList<ToStart> Admit(QueuedItem incoming)
            {
                // Cancel the item we're superseding via its own cancellation — no parallel
                // CancellationTokenSource of our own to create, own, or dispose. Safe even if
                // that item already finished on its own.
                if (this.active != null)
                {
                    this.Cancel(this.active);
                }

                this.active = incoming;

                return new[] { new ToStart(incoming) };
            }

            protected override StrategyResult OnCompleted(QueuedItem item, Outcome outcome)
            {
                // Only publish if nothing newer has since superseded this run.
                bool isCurrent = ReferenceEquals(objA: this.active, objB: item);

                // Drop the reference once the current run finishes, so we don't pin the last
                // QueuedItem (and its value) indefinitely after everything has gone idle.
                if (isCurrent)
                {
                    this.active = null;
                }

                return new StrategyResult(publish: isCurrent, next: StrategyResult.None);
            }
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
    Subclass AsyncConcurrencyStrategy<TInput, TResult> and implement Admit/OnCompleted as pure reporting —
    they have no access to the result/error sinks and don't start Tasks themselves; they just
    describe what should happen and let the base class carry it out. The one thing you can do
    imperatively is Cancel(item), to cancel an item you're managing (queued or running); it
    still completes normally through OnCompleted as Outcome.Cancelled, so you can chain from
    there. Every value you don't immediately start stays tracked as Queued automatically; hold
    onto its QueuedItem (not just its Value) if you intend to promote it later or recognize it
    again in OnCompleted — QueuedItem is a sealed class you can't construct, and identity is
    reference identity, so ReferenceEquals (as SwitchLatest does) is enough for "is this still
    current?" checks, with no separate handle needed. A strategy that neither starts nor
    remembers an incoming value leaves it permanently Queued — if you want a "reject outright"
    behavior, promote it immediately and have your own logic complete it right away instead.
*/