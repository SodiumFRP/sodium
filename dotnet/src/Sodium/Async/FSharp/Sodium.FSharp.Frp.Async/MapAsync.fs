module Sodium.Frp.Async

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Sodium.Frp
open Sodium.Frp.Async

// TState is never part of a strategy's public type (only TInput/TResult are), so ParallelStrategy
// is free to use something other than literal `unit` for it — which it must: F# can't override an
// abstract member whose signature reduces to a bare `unit -> unit` segment (ambiguous between "a
// nullary member" and "a member taking a unit argument"), and ParallelStrategy.CreateState is
// exactly that shape when TState is also `unit`.
[<Struct>]
type EmptyState = EmptyState

[<AbstractClass>]
type AsyncConcurrencyStrategy<'TInput, 'TState> =
    inherit AsyncConcurrencyStrategy<'TInput, unit, 'TState>

[<AbstractClass>]
type AsyncConcurrencyStrategy<'TState> =
    inherit AsyncConcurrencyStrategy<unit, unit, 'TState>

[<AbstractClass>]
type AsyncConcurrencyStrategy =
    inherit AsyncConcurrencyStrategy<unit, unit, EmptyState>

let private parallelInstance = AsyncConcurrencyStrategyFactory.Parallel()
let private queueInstance = AsyncConcurrencyStrategyFactory.Queue<unit>()
let private switchLatestInstance = AsyncConcurrencyStrategyFactory.SwitchLatest()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let parallelStrategy () : AsyncConcurrencyStrategyBase<unit, unit> = parallelInstance

[<MethodImpl(MethodImplOptions.NoInlining)>]
let queue () : AsyncConcurrencyStrategyBase<unit, unit> = queueInstance

[<MethodImpl(MethodImplOptions.NoInlining)>]
let switchLatest () : AsyncConcurrencyStrategyBase<unit, unit> = switchLatestInstance

[<MethodImpl(MethodImplOptions.NoInlining)>]
let queuePerGroupWithComparer
    (groupComparer : IEqualityComparer<'TGroup>)
    (getGroup : 'TInput -> 'TGroup) =
    AsyncConcurrencyStrategyFactory.QueuePerGroup<unit, _, _>(getGroup, groupComparer)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let queuePerGroup (getGroup : 'TInput -> 'TGroup) =
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