using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using JetBrains.Annotations;
using Sodium.Functional;

namespace Sodium.Frp.Async;

    /// <summary>
    ///     Extension methods that bridge an impure asynchronous operation into the FRP world:
    ///     listen on a Stream&lt;TInput&gt;, run an async operation per firing, push the result into a
    ///     StreamSink&lt;TResult&gt;, and expose what's queued/running — optionally wired up to streams
    ///     that trigger cancellation of queued or running work alike. The returned
    ///     <see cref="AsyncMapStatus{TInput}" /> is IDisposable; disposing it is how you tear the whole
    ///     pipeline down.
    ///     <para>
    ///         The overloads differ only in how the call's own
    ///         <c>TInput</c>/<c>TResult</c> reach the types <paramref name="strategy" /> is written
    ///         against. Pick by what your strategy needs: one that only schedules (Parallel, Queue,
    ///         SwitchLatest) ignores both, so neither converter is needed; one that inspects the input
    ///         (QueuePerGroup) needs <c>TInput</c> to either already be its input type or be
    ///         convertible to it. Every overload forwards to the last one, which takes both converters
    ///         explicitly and imposes no relationship at all.
    ///     </para>
    /// </summary>
    [PublicAPI]
    public static class AsyncStreamExtensions
    {
        /// <summary>
        ///     Convenience overload for a strategy that only cares about scheduling, not about
        ///     <typeparamref name="TInput" />/<typeparamref name="TResult" /> themselves (e.g. Parallel,
        ///     Queue, SwitchLatest) — both are erased to <see cref="Unit" /> before reaching it. See the
        ///     canonical
        ///     <see cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
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
        ///     Note that disposing never gags the pipeline: whatever is still in flight runs to
        ///     completion and still publishes to <paramref name="results" />/<paramref name="errors" />
        ///     after the call returns. With true that's usually moot, since canceled outcomes are
        ///     never published and an operation that honors its token won't produce one — but an
        ///     operation that ignores its token will, and with false it's the entire point.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult>(
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
                cancelAll: cancelAll?.MapImpl(_ => UnitInternal.Value),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload for a strategy that cares about <typeparamref name="TStrategyInput" />
        ///     but not about the result (<see cref="Unit" />), where <typeparamref name="TInput" /> is
        ///     already a <typeparamref name="TStrategyInput" /> (e.g. QueuePerGroup on the call's own
        ///     input type). See the canonical
        ///     <see cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
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
        ///     Note that disposing never gags the pipeline: whatever is still in flight runs to
        ///     completion and still publishes to <paramref name="results" />/<paramref name="errors" />
        ///     after the call returns. With true that's usually moot, since canceled outcomes are
        ///     never published and an operation that honors its token won't produce one — but an
        ///     operation that ignores its token will, and with false it's the entire point.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput>(
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
                cancelAll: cancelAll?.MapImpl(_ => UnitInternal.Value),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload for a strategy that cares about <typeparamref name="TStrategyInput" />
        ///     but not about the result (<see cref="Unit" />), where <paramref name="inputConverter" />
        ///     derives it from <typeparamref name="TInput" /> (e.g. QueuePerGroup, deriving the group
        ///     key). See the canonical
        ///     <see
        ///         cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
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
        ///     Note that disposing never gags the pipeline: whatever is still in flight runs to
        ///     completion and still publishes to <paramref name="results" />/<paramref name="errors" />
        ///     after the call returns. With true that's usually moot, since canceled outcomes are
        ///     never published and an operation that honors its token won't produce one — but an
        ///     operation that ignores its token will, and with false it's the entire point.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput>(
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
                cancelAll: cancelAll?.MapImpl(_ => UnitInternal.Value),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload for a strategy that doesn't care about the input (<see cref="Unit" />)
        ///     but does care about <typeparamref name="TStrategyResult" />, where
        ///     <typeparamref name="TResult" /> is already a <typeparamref name="TStrategyResult" />. See
        ///     the canonical
        ///     <see
        ///         cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
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
        ///     Note that disposing never gags the pipeline: whatever is still in flight runs to
        ///     completion and still publishes to <paramref name="results" />/<paramref name="errors" />
        ///     after the call returns. With true that's usually moot, since canceled outcomes are
        ///     never published and an operation that honors its token won't produce one — but an
        ///     operation that ignores its token will, and with false it's the entire point.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyResult>(
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
                cancelAll: cancelAll?.MapImpl(_ => UnitInternal.Value),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload for a strategy that doesn't care about the input (<see cref="Unit" />)
        ///     but does care about <typeparamref name="TStrategyResult" />, where
        ///     <paramref name="resultConverter" /> derives it from <typeparamref name="TResult" />. See
        ///     the canonical
        ///     <see
        ///         cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
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
        ///     Note that disposing never gags the pipeline: whatever is still in flight runs to
        ///     completion and still publishes to <paramref name="results" />/<paramref name="errors" />
        ///     after the call returns. With true that's usually moot, since canceled outcomes are
        ///     never published and an operation that honors its token won't produce one — but an
        ///     operation that ignores its token will, and with false it's the entire point.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyResult>(
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
                cancelAll: cancelAll?.MapImpl(_ => UnitInternal.Value),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

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
        ///     Note that disposing never gags the pipeline: whatever is still in flight runs to
        ///     completion and still publishes to <paramref name="results" />/<paramref name="errors" />
        ///     after the call returns. With true that's usually moot, since canceled outcomes are
        ///     never published and an operation that honors its token won't produce one — but an
        ///     operation that ignores its token will, and with false it's the entire point.
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
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput, TStrategyResult>(
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
                cancelAll: cancelAll?.MapImpl(_ => UnitInternal.Value),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload where <paramref name="inputConverter" /> derives
        ///     <typeparamref name="TStrategyInput" /> from <typeparamref name="TInput" />, but
        ///     <typeparamref name="TResult" /> is already a <typeparamref name="TStrategyResult" />. See
        ///     the canonical
        ///     <see
        ///         cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
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
        ///     Note that disposing never gags the pipeline: whatever is still in flight runs to
        ///     completion and still publishes to <paramref name="results" />/<paramref name="errors" />
        ///     after the call returns. With true that's usually moot, since canceled outcomes are
        ///     never published and an operation that honors its token won't produce one — but an
        ///     operation that ignores its token will, and with false it's the entire point.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput, TStrategyResult>(
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
                cancelAll: cancelAll?.MapImpl(_ => UnitInternal.Value),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     Convenience overload where <typeparamref name="TInput" /> is already a
        ///     <typeparamref name="TStrategyInput" />, but <paramref name="resultConverter" /> derives
        ///     <typeparamref name="TStrategyResult" /> from <typeparamref name="TResult" />. See the
        ///     canonical
        ///     <see
        ///         cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
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
        ///     Note that disposing never gags the pipeline: whatever is still in flight runs to
        ///     completion and still publishes to <paramref name="results" />/<paramref name="errors" />
        ///     after the call returns. With true that's usually moot, since canceled outcomes are
        ///     never published and an operation that honors its token won't produce one — but an
        ///     operation that ignores its token will, and with false it's the entire point.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput, TStrategyResult>(
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
                cancelAll: cancelAll?.MapImpl(_ => UnitInternal.Value),
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
        ///     Note that disposing never gags the pipeline: whatever is still in flight runs to
        ///     completion and still publishes to <paramref name="results" />/<paramref name="errors" />
        ///     after the call returns. With true that's usually moot, since canceled outcomes are
        ///     never published and an operation that honors its token won't produce one — but an
        ///     operation that ignores its token will, and with false it's the entire point.
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
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, TStrategyResult> strategy,
            Func<TInput, TStrategyInput> inputConverter,
            Func<TResult, TStrategyResult> resultConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true) =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: inputConverter,
                resultConverter: resultConverter,
                cancelAll: cancelAll?.MapImpl(_ => UnitInternal.Value),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);
    }