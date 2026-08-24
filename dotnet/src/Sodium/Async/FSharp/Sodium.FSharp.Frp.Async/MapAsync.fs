module Sodium.Frp.Async

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Sodium.Frp
open Sodium.Frp.Async

// This project's own copy of the built-in strategies (Parallel, Queue, QueuePerGroup,
// SwitchLatest), typed against F#'s native `unit` instead of the internal-to-Core UnitInternal —
// mirroring what the C# wrapper does against Sodium.Functional.Unit. Core has no publicly-usable
// "don't care" type of its own to expose these against, so each language wrapper provides its own.

// TState is never part of a strategy's public type (only TInput/TResult are), so ParallelStrategy
// is free to use something other than literal `unit` for it — which it must: F# can't override an
// abstract member whose signature reduces to a bare `unit -> unit` segment (ambiguous between "a
// nullary member" and "a member taking a unit argument"), and ParallelStrategy.CreateState is
// exactly that shape when TState is also `unit`.
[<Struct>]
type internal EmptyState =
    | EmptyState

type internal ParallelStrategy private () =
    inherit AsyncConcurrencyStrategy<unit, unit, EmptyState>()

    static member val Instance = ParallelStrategy()

    override _.CreateState() = EmptyState

    override _.Admit(_state, incoming) =
        [| AsyncMapBase.AsyncToStart<unit>(incoming) |] :> IReadOnlyList<_>

    override _.OnCompleted(_state, _item, _outcome) =
        AsyncMapBase.AsyncStrategyResult<unit>(publish = true, next = AsyncMapBase.AsyncStrategyResult<unit>.None)

type internal QueueState() =
    member val Pending = Queue<AsyncMapBase.AsyncQueuedItem<unit>>()
    member val Busy = false with get, set

type internal QueueStrategy private () =
    inherit AsyncConcurrencyStrategy<unit, unit, QueueState>()

    static member val Instance = QueueStrategy()

    override _.CreateState() = QueueState()

    override _.Admit(state, incoming) =
        if state.Busy then
            // Stays visible as Queued; still cancellable while it waits.
            state.Pending.Enqueue incoming
            AsyncMapBase.AsyncStrategyResult<unit>.None
        else
            state.Busy <- true
            [| AsyncMapBase.AsyncToStart<unit>(incoming) |] :> IReadOnlyList<_>

    override _.OnCompleted(state, _item, _outcome) =
        if state.Pending.Count > 0 then
            let next = state.Pending.Dequeue ()

            // If `next` was canceled while it sat here, the execution engine will notice when
            // promoting it and short-circuit straight to Outcome.Canceled(), which calls back
            // into OnCompleted and naturally dequeues whatever comes after it.
            AsyncMapBase.AsyncStrategyResult<unit>(
                publish = true,
                next = ([| AsyncMapBase.AsyncToStart<unit>(next) |] :> IReadOnlyList<_>))
        else
            state.Busy <- false
            AsyncMapBase.AsyncStrategyResult<unit>(publish = true, next = AsyncMapBase.AsyncStrategyResult<unit>.None)

type internal QueuePerGroupGroupState<'TInput>() =
    member val Pending = Queue<AsyncMapBase.AsyncQueuedItem<'TInput>>()
    member val Busy = false with get, set

/// Per-group queue state, keyed by group — groups are added on first use and removed once idle.
type internal QueuePerGroupState<'TInput, 'TGroup>(groupComparer : IEqualityComparer<'TGroup>) =
    member val Groups = Dictionary<'TGroup, QueuePerGroupGroupState<'TInput>>(groupComparer)

type internal QueuePerGroupStrategy<'TInput, 'TGroup>
    (getGroup : 'TInput -> 'TGroup, groupComparer : IEqualityComparer<'TGroup>) =
    inherit AsyncConcurrencyStrategy<'TInput, unit, QueuePerGroupState<'TInput, 'TGroup>>()

    override _.CreateState() = QueuePerGroupState(groupComparer)

    override _.Admit(state, incoming) =
        let group = getGroup incoming.Value

        let groupState =
            match state.Groups.TryGetValue group with
            | true, gs -> gs
            | false, _ ->
                let gs = QueuePerGroupGroupState()
                state.Groups.Add(group, gs)
                gs

        if groupState.Busy then
            // Stays visible as Queued; still cancellable while it waits.
            groupState.Pending.Enqueue incoming
            AsyncMapBase.AsyncStrategyResult<'TInput>.None
        else
            groupState.Busy <- true
            [| AsyncMapBase.AsyncToStart<'TInput>(incoming) |] :> IReadOnlyList<_>

    override _.OnCompleted(state, item, _outcome) =
        let group = getGroup item.Value

        let groupState =
            match state.Groups.TryGetValue group with
            | true, gs -> gs
            | false, _ -> failwith "Could not find group."

        if groupState.Pending.Count > 0 then
            let next = groupState.Pending.Dequeue ()

            // If `next` was canceled while it sat here, the execution engine will notice when
            // promoting it and short-circuit straight to Outcome.Canceled(), which calls back
            // into OnCompleted and naturally dequeues whatever comes after it.
            AsyncMapBase.AsyncStrategyResult<'TInput>(
                publish = true,
                next = ([| AsyncMapBase.AsyncToStart<'TInput>(next) |] :> IReadOnlyList<_>))
        else
            groupState.Busy <- false
            state.Groups.Remove group |> ignore
            AsyncMapBase.AsyncStrategyResult<'TInput>(publish = true, next = AsyncMapBase.AsyncStrategyResult<'TInput>.None)

type internal SwitchLatestState() =
    member val Active : AsyncMapBase.AsyncQueuedItem<unit> option = None with get, set

type internal SwitchLatestStrategy private () =
    inherit AsyncConcurrencyStrategy<unit, unit, SwitchLatestState>()

    static member val Instance = SwitchLatestStrategy()

    override _.CreateState() = SwitchLatestState()

    override _.Admit(state, incoming) =
        // Cancel the item we're superseding via its own cancellation — no parallel
        // CancellationTokenSource of our own to create, own, or dispose. Safe even if that item
        // already finished on its own.
        state.Active |> Option.iter (fun a -> a.Cancel())
        state.Active <- Some incoming
        [| AsyncMapBase.AsyncToStart<unit>(incoming) |] :> IReadOnlyList<_>

    override _.OnCompleted(state, item, _outcome) =
        // Only publish if nothing newer has since superseded this run.
        let isCurrent =
            match state.Active with
            | Some active -> active.Id = item.Id
            | None -> false

        // Drop the reference once the current run finishes, so we don't pin the last QueuedItem
        // (and its value) indefinitely after everything has gone idle.
        if isCurrent then
            state.Active <- None

        AsyncMapBase.AsyncStrategyResult<unit>(publish = isCurrent, next = AsyncMapBase.AsyncStrategyResult<unit>.None)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let runParallel () : AsyncConcurrencyStrategyBase<unit, unit> = ParallelStrategy.Instance

[<MethodImpl(MethodImplOptions.NoInlining)>]
let queue () : AsyncConcurrencyStrategyBase<unit, unit> = QueueStrategy.Instance

[<MethodImpl(MethodImplOptions.NoInlining)>]
let switchLatest () : AsyncConcurrencyStrategyBase<unit, unit> = SwitchLatestStrategy.Instance

[<MethodImpl(MethodImplOptions.NoInlining)>]
let queuePerGroupWithComparer
    (groupComparer : IEqualityComparer<'TGroup>)
    (getGroup : 'TInput -> 'TGroup) : AsyncConcurrencyStrategyBase<'TInput, unit> =
    QueuePerGroupStrategy(getGroup, groupComparer)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let queuePerGroup (getGroup : 'TInput -> 'TGroup) : AsyncConcurrencyStrategyBase<'TInput, unit> =
    getGroup |> queuePerGroupWithComparer EqualityComparer<'TGroup>.Default

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private toUnitInternalStream (cancelAll : Stream<unit> option) : Stream<UnitInternal> =
    match cancelAll with
    | Some s -> s.MapImpl(Func<_, _>(fun (_ : unit) -> UnitInternal.Value))
    | None -> null

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
