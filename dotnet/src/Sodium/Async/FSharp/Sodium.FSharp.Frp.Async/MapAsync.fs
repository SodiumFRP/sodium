module Sodium.Frp.Async

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Sodium.Frp
open Sodium.Frp.Async
open Sodium.Functional

[<MethodImpl(MethodImplOptions.NoInlining)>]
let mapAsync
    (results : StreamSink<'TResult>)
    (errors : StreamSink<exn>)
    (operation : 'TInput -> CancellationToken -> Task<'TResult>)
    (strategy : AsyncConcurrencyStrategyBase<Unit, Unit>)
    (cancelAll : Stream<Unit> option)
    (cancelMatching : Stream<IReadOnlyCollection<'TInput>> option)
    (cancelOnDispose : bool)
    (source : Stream<'TInput>) : AsyncMapStatus<'TInput> =
    AsyncStreamUtility.MapAsyncImpl<'TInput, 'TResult> (
        source,
        results,
        errors,
        Func<_, _, _> operation,
        strategy,
        (cancelAll |> Option.toObj),
        (cancelMatching |> Option.toObj),
        cancelOnDispose)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let mapAsyncWithInputConverter
    (results : StreamSink<'TResult>)
    (errors : StreamSink<exn>)
    (operation : 'TInput -> CancellationToken -> Task<'TResult>)
    (strategy : AsyncConcurrencyStrategyBase<'TStrategyInput, Unit>)
    (inputConverter : 'TInput -> 'TStrategyInput)
    (cancelAll : Stream<Unit> option)
    (cancelMatching : Stream<IReadOnlyCollection<'TInput>> option)
    (cancelOnDispose : bool)
    (source : Stream<'TInput>) : AsyncMapStatus<'TInput> =
    AsyncStreamUtility.MapAsyncImpl<'TInput, 'TResult, 'TStrategyInput> (
        source,
        results,
        errors,
        Func<_, _, _> operation,
        strategy,
        Func<_, _> inputConverter,
        (cancelAll |> Option.toObj),
        (cancelMatching |> Option.toObj),
        cancelOnDispose)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let mapAsyncWithResultConverter
    (results : StreamSink<'TResult>)
    (errors : StreamSink<exn>)
    (operation : 'TInput -> CancellationToken -> Task<'TResult>)
    (strategy : AsyncConcurrencyStrategyBase<Unit, 'TStrategyResult>)
    (resultConverter : 'TResult -> 'TStrategyResult)
    (cancelAll : Stream<Unit> option)
    (cancelMatching : Stream<IReadOnlyCollection<'TInput>> option)
    (cancelOnDispose : bool)
    (source : Stream<'TInput>) : AsyncMapStatus<'TInput> =
    AsyncStreamUtility.MapAsyncImpl<'TInput, 'TResult, 'TStrategyResult> (
        source,
        results,
        errors,
        Func<_, _, _> operation,
        strategy,
        Func<_, _> resultConverter,
        (cancelAll |> Option.toObj),
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
    (cancelAll : Stream<Unit> option)
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
        (cancelAll |> Option.toObj),
        (cancelMatching |> Option.toObj),
        cancelOnDispose)
