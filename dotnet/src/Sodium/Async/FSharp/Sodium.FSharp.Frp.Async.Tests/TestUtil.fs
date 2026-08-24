module Sodium.Frp.Async.Tests.TestUtil

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.Threading
open System.Threading.Tasks

/// Polls `condition` until it's true, or fails the test via timeout.
let waitUntil (condition : unit -> bool) =
    let sw = Stopwatch.StartNew()
    while not (condition ()) do
        if sw.ElapsedMilliseconds > 5000L then
            raise (TimeoutException("Condition was not met within the timeout."))
        Thread.Sleep(10)

/// An async operation, keyed by input, that a test controls the completion of explicitly via
/// Release/Fail rather than racing real time. Also records which inputs have actually been
/// invoked, so a test can assert an operation started running (rather than merely being admitted)
/// before releasing it.
type ControlledOperation<'TInput, 'TResult when 'TInput : equality>() =
    let gates = ConcurrentDictionary<'TInput, TaskCompletionSource<'TResult>>()
    let started = ConcurrentDictionary<'TInput, bool>()

    let gateFor input = gates.GetOrAdd(input, fun _ -> TaskCompletionSource<'TResult>())

    member _.HasStarted(input : 'TInput) = started.ContainsKey(input)

    member _.Release(input : 'TInput, result : 'TResult) =
        (gateFor input).TrySetResult(result) |> ignore

    member _.Fail(input : 'TInput, error : exn) =
        (gateFor input).TrySetException(error) |> ignore

    member _.Operation : 'TInput -> CancellationToken -> Task<'TResult> =
        fun input token ->
            started.[input] <- true
            let tcs = gateFor input
            token.Register(fun () -> tcs.TrySetCanceled(token) |> ignore) |> ignore
            tcs.Task
