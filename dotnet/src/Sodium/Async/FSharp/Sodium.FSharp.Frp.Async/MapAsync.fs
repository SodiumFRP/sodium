/// <summary>
///     Bridges an impure asynchronous operation into the FRP world: listen on a
///     <c>Stream&lt;'TInput&gt;</c>, run an async operation per firing, push the result into a
///     <c>StreamSink&lt;'TResult&gt;</c>, and expose what's queued or running — optionally wired up to
///     streams that trigger cancellation of queued and running work alike. The
///     <c>AsyncMapStatus&lt;'TInput&gt;</c> every function here returns is IDisposable; disposing it is
///     how you tear the whole pipeline down.
/// </summary>
/// <remarks>
///     The F# equivalent of Sodium.Frp.Async's C# AsyncStreamExtensions/AsyncConcurrencyStrategy,
///     typed against F#'s native <c>unit</c> rather than <c>Sodium.Functional.Unit</c> wherever a
///     "don't care" type is needed. Since F# has neither overloading nor optional parameters on
///     let-bound functions, what C# expresses as nine MapAsync overloads is four distinctly named
///     functions here, and the cancellation arguments are explicit rather than defaulted.
/// </remarks>
module Sodium.Frp.Async

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Sodium.Frp
open Sodium.Frp.Async

// The three shorthand classes below mirror the C# wrapper's own AsyncConcurrencyStrategy /
// AsyncConcurrencyStrategy<TState> / AsyncConcurrencyStrategy<TInput,TState>. They're unrelated to
// the built-in strategies further down, which call straight into Core's shared, generic
// AsyncConcurrencyStrategyFactory instead of going through these.

/// <summary>
///     The state type for a custom strategy that needs no state of its own.
/// </summary>
/// <remarks>
///     Exists because F# cannot override an abstract member whose signature reduces to a bare
///     <c>unit -&gt; unit</c> segment — it's ambiguous between "a nullary member" and "a member
///     taking a unit argument" — which is exactly what <c>CreateState</c> would be if a strategy
///     used <c>unit</c> as its own state type. Naming a distinct type sidesteps that, which is why
///     the non-generic <see cref="T:Sodium.Frp.Async.AsyncConcurrencyStrategy" /> below is fixed to
///     this rather than to <c>unit</c>.
/// </remarks>
[<Struct>]
type EmptyState = EmptyState

/// <summary>
///     Shorthand base class for a custom strategy that inspects its input but publishes no
///     meaningful result — the result type is fixed to <c>unit</c>. Saves spelling out <c>unit</c>
///     when subclassing Core's three-parameter AsyncConcurrencyStrategy directly.
/// </summary>
/// <typeparam name="TInput">The input type this strategy schedules by.</typeparam>
/// <typeparam name="TState">
///     The per-call scheduling state this strategy manages; opaque to callers, and never part of a
///     mapAsync signature.
/// </typeparam>
[<AbstractClass>]
type AsyncConcurrencyStrategy<'TInput, 'TState>() =
    inherit AsyncConcurrencyStrategy<'TInput, unit, 'TState>()

/// <summary>
///     Shorthand base class for a custom strategy that cares about neither the input nor the
///     result — both are fixed to <c>unit</c> — but does keep scheduling state of its own.
/// </summary>
/// <typeparam name="TState">
///     The per-call scheduling state this strategy manages; opaque to callers, and never part of a
///     mapAsync signature.
/// </typeparam>
[<AbstractClass>]
type AsyncConcurrencyStrategy<'TState>() =
    inherit AsyncConcurrencyStrategy<unit, unit, 'TState>()

/// <summary>
///     Shorthand base class for a custom strategy that cares about neither the input nor the result
///     and keeps no state of its own — see <see cref="T:Sodium.Frp.Async.EmptyState" /> for why the
///     state type is named rather than being <c>unit</c>.
/// </summary>
[<AbstractClass>]
type AsyncConcurrencyStrategy() =
    inherit AsyncConcurrencyStrategy<unit, unit, EmptyState>()

let private parallelInstance = AsyncConcurrencyStrategyFactory.Parallel()
let private queueInstance = AsyncConcurrencyStrategyFactory.Queue<unit>()
let private switchLatestInstance = AsyncConcurrencyStrategyFactory.SwitchLatest()

/// <summary>
///     Every firing starts its own operation immediately; results arrive in completion order.
/// </summary>
/// <returns>
///     A strategy for <c>mapAsync</c>. Holds no state of its own, so the same instance is safe to
///     pass to any number of <c>mapAsync</c> calls, even concurrently — each call gets its own
///     independent scheduling state.
/// </returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let parallelStrategy () : AsyncConcurrencyStrategyBase<unit, unit> = parallelInstance

/// <summary>
///     At most one operation runs at a time; later firings queue and run in order.
/// </summary>
/// <returns>
///     A strategy for <c>mapAsync</c>, reusable across calls on the same terms as
///     <c>parallelStrategy</c>.
/// </returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let queueStrategy () : AsyncConcurrencyStrategyBase<unit, unit> = queueInstance

/// <summary>
///     A new firing cancels whatever is currently in flight and takes its place. The superseded
///     run's result is never published, whether or not its operation honors the cancellation token.
/// </summary>
/// <returns>
///     A strategy for <c>mapAsync</c>, reusable across calls on the same terms as
///     <c>parallelStrategy</c>.
/// </returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let switchLatestStrategy () : AsyncConcurrencyStrategyBase<unit, unit> = switchLatestInstance

/// <summary>
///     One independent queue per group: within a group, later firings queue behind earlier ones
///     exactly as <c>queueStrategy</c> does, but different groups don't wait on each other. As
///     <c>queuePerGroupStrategy</c>, but with an explicit comparer for the group keys.
/// </summary>
/// <param name="groupComparer">Equality comparer used to match group keys.</param>
/// <param name="getGroup">
///     Computes the group key for an input value. Must be deterministic: it's called once when the
///     value is admitted and again when it completes, and both calls must agree for the item to be
///     matched back to the queue it was placed in.
/// </param>
/// <returns>
///     A strategy for <c>mapAsyncWithInputConverter</c> — it inspects the input, so it can't be
///     passed to plain <c>mapAsync</c>. Reusable across calls on the same terms as
///     <c>parallelStrategy</c>.
/// </returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let queuePerGroupStrategyWithComparer
    (groupComparer : IEqualityComparer<'TGroup>)
    (getGroup : 'TInput -> 'TGroup) =
    AsyncConcurrencyStrategyFactory.QueuePerGroup<unit, _, _>(getGroup, groupComparer)

/// <summary>
///     One independent queue per group, keyed by the default equality comparer for
///     <c>'TGroup</c> — see <c>queuePerGroupStrategyWithComparer</c> to supply your own.
/// </summary>
/// <param name="getGroup">
///     Computes the group key for an input value. Must be deterministic; see
///     <c>queuePerGroupStrategyWithComparer</c>.
/// </param>
/// <returns>
///     A strategy for <c>mapAsyncWithInputConverter</c> — it inspects the input, so it can't be
///     passed to plain <c>mapAsync</c>. Reusable across calls on the same terms as
///     <c>parallelStrategy</c>.
/// </returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let queuePerGroupStrategy (getGroup : 'TInput -> 'TGroup) =
    getGroup |> queuePerGroupStrategyWithComparer EqualityComparer<'TGroup>.Default

// Core's MapAsyncImpl takes its cancelAll as a Stream<UnitInternal>, since Core has no "don't
// care" type it can expose. Mapping rather than casting is what keeps UnitInternal — which is
// internal to Sodium.Core.Frp and so unnameable by anyone consuming this library — out of every
// signature in this module. `null` for None: Core's parameter is a plain nullable reference.
[<MethodImpl(MethodImplOptions.NoInlining)>]
let private toUnitInternalStream (cancelAll : Stream<unit> option) : Stream<UnitInternal> =
    match cancelAll with
    | Some s -> s.MapImpl(Func<_, _>(fun (_ : unit) -> UnitInternal.Value))
    | None -> null

// The four mapAsync functions below differ only in how this call's own 'TInput/'TResult reach the
// types `strategy` is written against — exactly the axis the C# wrapper's overloads vary along, but
// spelled as distinct names since F# has no optional/overloaded let bindings:
//
//   mapAsync                     strategy ignores both       (parallelStrategy, queueStrategy,
//                                                             switchLatestStrategy)
//   mapAsyncWithInputConverter   strategy inspects the input (queuePerGroupStrategy)
//   mapAsyncWithResultConverter  strategy inspects the result
//   mapAsyncWithConverters       strategy inspects both
//
// Deliberately absent are variants keyed on 'TInput already being a subtype of the strategy's input
// type: F# can't express that constraint between two open type parameters, and `fun v -> v` as the
// converter covers it anyway. `source` comes last throughout so these compose with |>. cancelAll,
// cancelMatching and cancelOnDispose are all required arguments rather than optional as in C# —
// pass None, None and true for the common case.

/// <summary>
///     Runs <paramref name="operation" /> for each firing of <paramref name="source" />, sending
///     successes to <paramref name="results" /> and failures to <paramref name="errors" />, with
///     <paramref name="strategy" /> deciding what runs when. For a strategy that ignores both the
///     input and the result — <c>parallelStrategy</c>, <c>queueStrategy</c>,
///     <c>switchLatestStrategy</c>.
/// </summary>
/// <param name="results">
///     Each successful operation's return value is sent here, in completion order rather than the
///     order inputs arrived. A result whose run was superseded or canceled is not sent.
/// </param>
/// <param name="errors">
///     Every failed operation is sent here — there is deliberately no way to call this without
///     somewhere for errors to go.
/// </param>
/// <param name="operation">
///     The asynchronous work to run per input. Invoked inline, so it doesn't reach a thread pool
///     until it awaits something itself, and it's handed a CancellationToken combining this item's
///     own cancellation with any token the strategy supplied. Honoring that token is what makes
///     <paramref name="cancelAll" />, <paramref name="cancelMatching" /> and
///     <paramref name="cancelOnDispose" /> take effect on work that has already started — an
///     operation that ignores it still runs to completion, and cancellation only means its result
///     goes unpublished.
/// </param>
/// <param name="strategy">
///     How overlapping requests are handled. Holds no state of its own and may safely be reused
///     across calls, even concurrently.
/// </param>
/// <param name="cancelAll">
///     <c>Some</c> stream whose every firing cancels every tracked operation, queued or already
///     running, or <c>None</c>. A queued item that's canceled is simply never started when its turn
///     comes; a running one stops only if its operation observes its CancellationToken.
/// </param>
/// <param name="cancelMatching">
///     <c>Some</c> stream whose every firing cancels whichever tracked operations, queued or
///     running, were admitted for an input value present in the fired collection (compared with the
///     default equality comparer for <c>'TInput</c>), or <c>None</c>. Same caveats as
///     <paramref name="cancelAll" />.
/// </param>
/// <param name="cancelOnDispose">
///     Whether disposing the returned status also cancels every item tracked at that point, queued
///     or running. Either way, disposing always, unconditionally, stops any further values from
///     ever being admitted. Note that disposing never gags the pipeline: whatever is still in
///     flight runs to completion and still publishes to <paramref name="results" /> or
///     <paramref name="errors" /> afterwards.
/// </param>
/// <param name="source">
///     The stream of inputs to run against. Every firing is offered to
///     <paramref name="strategy" />, which decides whether it starts immediately or waits. After
///     the returned status is disposed, further firings are ignored entirely.
/// </param>
/// <returns>
///     An <c>AsyncMapStatus&lt;'TInput&gt;</c>: <c>IsRunning</c> is a <c>Cell&lt;bool&gt;</c> that is
///     true while at least one invocation is actually running (not merely queued), updating
///     glitch-free in the same transaction as whichever event caused it to change; <c>Items</c>
///     lists every tracked value with its status; disposing it tears the pipeline down.
/// </returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let mapAsync
    (results : StreamSink<'TResult>)
    (errors : StreamSink<exn>)
    (operation : 'TInput -> CancellationToken -> Task<'TResult>)
    (strategy : AsyncConcurrencyStrategyBase<unit, unit>)
    (cancelAll : Stream<unit> option)
    (cancelMatching : Stream<IReadOnlyCollection<'TInput>> option)
    (cancelOnDispose : bool)
    (source : Stream<'TInput>) : AsyncMapStatus<'TInput> =
    AsyncStreamUtility.MapAsyncImpl<'TInput, 'TResult, unit, unit> (
        source,
        results,
        errors,
        Func<_, _, _> operation,
        strategy,
        Func<_, _>(fun (_ : 'TInput) -> ()),
        Func<_, _>(fun (_ : 'TResult) -> ()),
        (cancelAll |> toUnitInternalStream),
        (cancelMatching |> Option.toObj),
        cancelOnDispose)

/// <summary>
///     As <c>mapAsync</c>, but for a strategy that inspects the input — <c>queuePerGroupStrategy</c>
///     and friends. <paramref name="inputConverter" /> derives the value the strategy is written
///     against; pass <c>fun v -&gt; v</c> where <c>'TInput</c> already is that type. See
///     <c>mapAsync</c> for the full contract of the parameters shared with it.
/// </summary>
/// <param name="results">Where each successful operation's return value is sent.</param>
/// <param name="errors">Where every failed operation is sent.</param>
/// <param name="operation">The asynchronous work to run per input.</param>
/// <param name="strategy">How overlapping requests are handled.</param>
/// <param name="inputConverter">
///     Converts each <c>'TInput</c> to the <c>'TStrategyInput</c> the strategy is written against,
///     before it's admitted.
/// </param>
/// <param name="cancelAll"><c>Some</c> stream cancelling every tracked operation, or <c>None</c>.</param>
/// <param name="cancelMatching">
///     <c>Some</c> stream cancelling tracked operations by input value, or <c>None</c>.
/// </param>
/// <param name="cancelOnDispose">
///     Whether disposing the returned status also cancels everything tracked at that point.
/// </param>
/// <param name="source">The stream of inputs to run against.</param>
/// <returns>
///     An <c>AsyncMapStatus&lt;'TInput&gt;</c> reporting what's queued and running; disposing it
///     tears the pipeline down.
/// </returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let mapAsyncWithInputConverter
    (results : StreamSink<'TResult>)
    (errors : StreamSink<exn>)
    (operation : 'TInput -> CancellationToken -> Task<'TResult>)
    (strategy : AsyncConcurrencyStrategyBase<'TStrategyInput, unit>)
    (inputConverter : 'TInput -> 'TStrategyInput)
    (cancelAll : Stream<unit> option)
    (cancelMatching : Stream<IReadOnlyCollection<'TInput>> option)
    (cancelOnDispose : bool)
    (source : Stream<'TInput>) : AsyncMapStatus<'TInput> =
    AsyncStreamUtility.MapAsyncImpl<'TInput, 'TResult, 'TStrategyInput, unit> (
        source,
        results,
        errors,
        Func<_, _, _> operation,
        strategy,
        Func<_, _> inputConverter,
        Func<_, _>(fun (_ : 'TResult) -> ()),
        (cancelAll |> toUnitInternalStream),
        (cancelMatching |> Option.toObj),
        cancelOnDispose)

/// <summary>
///     As <c>mapAsync</c>, but for a strategy that inspects the result rather than the input.
///     <paramref name="resultConverter" /> derives the value the strategy is written against; pass
///     <c>fun v -&gt; v</c> where <c>'TResult</c> already is that type. See <c>mapAsync</c> for the
///     full contract of the parameters shared with it.
/// </summary>
/// <param name="results">Where each successful operation's return value is sent.</param>
/// <param name="errors">Where every failed operation is sent.</param>
/// <param name="operation">The asynchronous work to run per input.</param>
/// <param name="strategy">How overlapping requests are handled.</param>
/// <param name="resultConverter">
///     Converts each successful <c>'TResult</c> to the <c>'TStrategyResult</c> the strategy is
///     written against, before the strategy is told the item completed. Not called for a failed or
///     canceled run, which have no result to convert.
/// </param>
/// <param name="cancelAll"><c>Some</c> stream cancelling every tracked operation, or <c>None</c>.</param>
/// <param name="cancelMatching">
///     <c>Some</c> stream cancelling tracked operations by input value, or <c>None</c>.
/// </param>
/// <param name="cancelOnDispose">
///     Whether disposing the returned status also cancels everything tracked at that point.
/// </param>
/// <param name="source">The stream of inputs to run against.</param>
/// <returns>
///     An <c>AsyncMapStatus&lt;'TInput&gt;</c> reporting what's queued and running; disposing it
///     tears the pipeline down.
/// </returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let mapAsyncWithResultConverter
    (results : StreamSink<'TResult>)
    (errors : StreamSink<exn>)
    (operation : 'TInput -> CancellationToken -> Task<'TResult>)
    (strategy : AsyncConcurrencyStrategyBase<unit, 'TStrategyResult>)
    (resultConverter : 'TResult -> 'TStrategyResult)
    (cancelAll : Stream<unit> option)
    (cancelMatching : Stream<IReadOnlyCollection<'TInput>> option)
    (cancelOnDispose : bool)
    (source : Stream<'TInput>) : AsyncMapStatus<'TInput> =
    AsyncStreamUtility.MapAsyncImpl<'TInput, 'TResult, unit, 'TStrategyResult> (
        source,
        results,
        errors,
        Func<_, _, _> operation,
        strategy,
        Func<_, _>(fun (_ : 'TInput) -> ()),
        Func<_, _> resultConverter,
        (cancelAll |> toUnitInternalStream),
        (cancelMatching |> Option.toObj),
        cancelOnDispose)

/// <summary>
///     As <c>mapAsync</c>, but for a strategy that inspects both the input and the result. The
///     fully general form the other three are special cases of — it imposes no relationship at all
///     between this call's <c>'TInput</c>/<c>'TResult</c> and the types the strategy is written
///     against, since both converters are supplied explicitly. See <c>mapAsync</c> for the full
///     contract of the parameters shared with it.
/// </summary>
/// <param name="results">Where each successful operation's return value is sent.</param>
/// <param name="errors">Where every failed operation is sent.</param>
/// <param name="operation">The asynchronous work to run per input.</param>
/// <param name="strategy">How overlapping requests are handled.</param>
/// <param name="inputConverter">
///     Converts each <c>'TInput</c> to the <c>'TStrategyInput</c> the strategy is written against,
///     before it's admitted.
/// </param>
/// <param name="resultConverter">
///     Converts each successful <c>'TResult</c> to the <c>'TStrategyResult</c> the strategy is
///     written against, before the strategy is told the item completed.
/// </param>
/// <param name="cancelAll"><c>Some</c> stream cancelling every tracked operation, or <c>None</c>.</param>
/// <param name="cancelMatching">
///     <c>Some</c> stream cancelling tracked operations by input value, or <c>None</c>.
/// </param>
/// <param name="cancelOnDispose">
///     Whether disposing the returned status also cancels everything tracked at that point.
/// </param>
/// <param name="source">The stream of inputs to run against.</param>
/// <returns>
///     An <c>AsyncMapStatus&lt;'TInput&gt;</c> reporting what's queued and running; disposing it
///     tears the pipeline down.
/// </returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let mapAsyncWithConverters
    (results : StreamSink<'TResult>)
    (errors : StreamSink<exn>)
    (operation : 'TInput -> CancellationToken -> Task<'TResult>)
    (strategy : AsyncConcurrencyStrategyBase<'TStrategyInput, 'TStrategyResult>)
    (inputConverter : 'TInput -> 'TStrategyInput)
    (resultConverter : 'TResult -> 'TStrategyResult)
    (cancelAll : Stream<unit> option)
    (cancelMatching : Stream<IReadOnlyCollection<'TInput>> option)
    (cancelOnDispose : bool)
    (source : Stream<'TInput>) : AsyncMapStatus<'TInput> =
    AsyncStreamUtility.MapAsyncImpl<'TInput, 'TResult, 'TStrategyInput, 'TStrategyResult> (
        source,
        results,
        errors,
        Func<_, _, _> operation,
        strategy,
        Func<_, _> inputConverter,
        Func<_, _> resultConverter,
        (cancelAll |> toUnitInternalStream),
        (cancelMatching |> Option.toObj),
        cancelOnDispose)