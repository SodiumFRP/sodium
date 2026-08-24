module Sodium.Frp.Async

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Sodium.Frp
open Sodium.Frp.Async

// These three shorthand classes mirror the C# wrapper's own AsyncConcurrencyStrategy /
// AsyncConcurrencyStrategy<TState> / AsyncConcurrencyStrategy<TInput,TState> — they exist purely so
// a consumer writing their own custom strategy against `unit` can subclass Core's
// AsyncConcurrencyStrategy<TInput,TResult,TState> without spelling out `unit` twice. They're
// unrelated to the built-in Parallel/Queue/SwitchLatest/QueuePerGroup below, which call straight
// into Core's shared, generic AsyncConcurrencyStrategyFactory instead of going through these.
//
// The non-generic AsyncConcurrencyStrategy needs a concrete TState (not `unit`) for the same
// reason ParallelStrategy does in Core: TState is never part of a strategy's public type (only
// TInput/TResult are), but F# can't override an abstract member whose signature reduces to a bare
// `unit -> unit` segment (ambiguous between "a nullary member" and "a member taking a unit
// argument") — which CreateState would be if TState were also `unit`. EmptyState sidesteps that
// for any consumer implementing CreateState against this shorthand.
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
let queueStrategy () : AsyncConcurrencyStrategyBase<unit, unit> = queueInstance

[<MethodImpl(MethodImplOptions.NoInlining)>]
let switchLatestStrategy () : AsyncConcurrencyStrategyBase<unit, unit> = switchLatestInstance

[<MethodImpl(MethodImplOptions.NoInlining)>]
let queuePerGroupStrategyWithComparer
    (groupComparer : IEqualityComparer<'TGroup>)
    (getGroup : 'TInput -> 'TGroup) =
    AsyncConcurrencyStrategyFactory.QueuePerGroup<unit, _, _>(getGroup, groupComparer)

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