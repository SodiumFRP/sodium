module Sodium.Frp.Async.Tests.MapAsyncTests

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open Sodium.Frp
open Sodium.Frp.Async
open Sodium.Frp.Async.Tests.TestUtil

// A handwritten custom strategy (subclassing AsyncConcurrencyStrategy<'TInput,'TResult,'TState>
// and overriding Admit/OnCompleted/CreateState) IS exercised below. An earlier version of this
// file concluded F# couldn't do this at all — that was wrong, and the actual causes were more
// mundane than "a genuine compiler limitation":
//   - This project had no direct ProjectReference to Sodium.Core.Frp.Async, only a transitive one
//     through Sodium.FSharp.Frp.Async. F#'s accessibility checking for the protected-internal
//     AsyncQueuedItem/AsyncToStart/AsyncOutcome/AsyncStrategyResult types needs the direct
//     reference to resolve correctly; a transitive one isn't enough.
//   - AsyncToStart<'T>, AsyncOutcome<'T> and AsyncStrategyResult<'T> were readonly structs. Every
//     struct has an implicit, non-suppressible parameterless constructor, and F# could reach it
//     (e.g. via a bare default/Unchecked.defaultof), silently producing an invalid instance — item
//     = null, in particular — without ever running the validating constructor. They're sealed
//     classes now, closing that hole; there's no default construction path around `new`.
//   - The F# module's own non-generic AsyncConcurrencyStrategy shorthand classes used the
//     abbreviated `type X = inherit Y` class syntax, which didn't reliably produce a constructor
//     usable from outside the module. They're written with an explicit `type X() = inherit Y()`
//     now.
//   - One more, found while porting these tests, with no C# equivalent to fix: F# rejects
//     constructing a protected-internal type directly inside an array literal (`[| Ctor(...) |]`)
//     as if that were itself a closure, even though nothing about it looks like one. Building the
//     instance in its own `let` first and only referencing that local inside the array — see
//     Admit below, in both strategies — is what actually resolves it.

/// Starts everything immediately, like parallelStrategy, but works against arbitrary
/// 'TStrategyInput/'TStrategyResult and records both what it was admitted with and what it saw on
/// completion — so a test can assert a converter actually ran, not merely compiled.
type private AlwaysStartStrategy<'TStrategyInput, 'TStrategyResult>() =
    inherit AsyncConcurrencyStrategy<'TStrategyInput, 'TStrategyResult, EmptyState>()

    let admittedValues = ResizeArray<'TStrategyInput>()
    let completedResults = ResizeArray<'TStrategyResult>()

    member _.AdmittedValues = admittedValues
    member _.CompletedResults = completedResults

    override _.CreateState() = EmptyState

    override _.Admit(_state : EmptyState, incoming : AsyncMapBase.AsyncQueuedItem<'TStrategyInput>) =
        // The protected-internal item's members can't be read from inside a closure — read the
        // value out to a plain local first, then close over that instead.
        let v = incoming.Value
        lock admittedValues (fun () -> admittedValues.Add(v))
        let toStart = AsyncMapBase.AsyncToStart<'TStrategyInput>(incoming)
        [| toStart |] :> IReadOnlyList<_>

    override _.OnCompleted(
        _state : EmptyState,
        _item : AsyncMapBase.AsyncQueuedItem<'TStrategyInput>,
        outcome : AsyncMapBase.AsyncOutcome<'TStrategyResult>) =
        let mutable captured = Unchecked.defaultof<'TStrategyResult>
        outcome.MatchVoid(Action<'TStrategyResult>(fun v -> captured <- v), null, null)
        lock completedResults (fun () -> completedResults.Add(captured))
        AsyncMapBase.AsyncStrategyResult<'TStrategyInput>(true, AsyncMapBase.AsyncStrategyResult<'TStrategyInput>.None)

/// A trivial custom strategy using EmptyState directly (input and result both fixed to `unit`,
/// via the F# module's own non-generic AsyncConcurrencyStrategy shorthand) — every value starts
/// immediately, like parallelStrategy, but also counts admissions.
type private CountingStrategy() =
    inherit AsyncConcurrencyStrategy()

    let mutable count = 0

    member _.AdmittedCount = count

    override _.CreateState() = EmptyState

    override _.Admit(_state : EmptyState, incoming : AsyncMapBase.AsyncQueuedItem<unit>) =
        count <- count + 1
        let toStart = AsyncMapBase.AsyncToStart<unit>(incoming)
        [| toStart |] :> IReadOnlyList<_>

    override _.OnCompleted(
        _state : EmptyState,
        _item : AsyncMapBase.AsyncQueuedItem<unit>,
        _outcome : AsyncMapBase.AsyncOutcome<unit>) =
        AsyncMapBase.AsyncStrategyResult<unit>(true, AsyncMapBase.AsyncStrategyResult<unit>.None)

[<TestFixture>]
type ``MapAsync Tests``() =

    [<Test>]
    member _.``parallelStrategy starts both immediately and publishes in completion order``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let op = ControlledOperation<string, string>()
        let received = List<string>()
        let l = results |> listenS received.Add

        let status =
            source |> mapAsync results errors op.Operation (parallelStrategy ()) None None true

        source |> sendS "a"
        source |> sendS "b"

        waitUntil (fun () -> op.HasStarted "a" && op.HasStarted "b")

        op.Release("b", "B")
        waitUntil (fun () -> received.Count = 1)
        op.Release("a", "A")
        waitUntil (fun () -> received.Count = 2)

        CollectionAssert.AreEqual([ "B"; "A" ], received)

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``queueStrategy runs one at a time in FIFO order``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let op = ControlledOperation<string, string>()
        let received = List<string>()
        let l = results |> listenS received.Add

        let status =
            source |> mapAsync results errors op.Operation (queueStrategy ()) None None true

        source |> sendS "a"
        source |> sendS "b"

        waitUntil (fun () -> op.HasStarted "a")
        Assert.IsFalse(op.HasStarted "b")

        op.Release("a", "A")
        waitUntil (fun () -> op.HasStarted "b")
        op.Release("b", "B")

        waitUntil (fun () -> received.Count = 2)
        CollectionAssert.AreEqual([ "A"; "B" ], received)

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``switchLatestStrategy never publishes a superseded run``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let op = ControlledOperation<string, string>()
        let received = List<string>()
        let l = results |> listenS received.Add

        let status =
            source |> mapAsync results errors op.Operation (switchLatestStrategy ()) None None true

        source |> sendS "a"
        waitUntil (fun () -> op.HasStarted "a")
        source |> sendS "b"
        waitUntil (fun () -> op.HasStarted "b")

        op.Release("a", "A")
        op.Release("b", "B")
        waitUntil (fun () -> received.Count = 1)

        Thread.Sleep(100)
        CollectionAssert.AreEqual([ "B" ], received)

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``queuePerGroupStrategy lets different groups run concurrently but serializes within a group``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let op = ControlledOperation<string, string>()
        let received = List<string>()
        let l = results |> listenS received.Add

        let getGroup (v : string) = v.Split('-').[0]
        let strategy = queuePerGroupStrategy getGroup

        let status =
            source |> mapAsyncWithInputConverter results errors op.Operation strategy id None None true

        source |> sendS "g1-a"
        source |> sendS "g1-b"
        source |> sendS "g2-a"

        waitUntil (fun () -> op.HasStarted "g1-a" && op.HasStarted "g2-a")
        Assert.IsFalse(op.HasStarted "g1-b")

        op.Release("g1-a", "A1")
        waitUntil (fun () -> op.HasStarted "g1-b")

        op.Release("g1-b", "B1")
        op.Release("g2-a", "A2")
        waitUntil (fun () -> received.Count = 3)

        CollectionAssert.AreEquivalent([ "A1"; "B1"; "A2" ], received)

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``queuePerGroupStrategyWithComparer uses the supplied comparer for group keys``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let op = ControlledOperation<string, string>()
        let received = List<string>()
        let l = results |> listenS received.Add

        // Case-insensitive grouping: "A-1" and "a-2" share a group despite differing case.
        let getGroup (v : string) = v.Split('-').[0]
        let strategy = queuePerGroupStrategyWithComparer StringComparer.OrdinalIgnoreCase getGroup

        let status =
            source |> mapAsyncWithInputConverter results errors op.Operation strategy id None None true

        source |> sendS "A-1"
        source |> sendS "a-2"

        waitUntil (fun () -> op.HasStarted "A-1")
        Assert.IsFalse(op.HasStarted "a-2", "a-2 shares a group with A-1 under a case-insensitive comparer.")

        op.Release("A-1", "R1")
        waitUntil (fun () -> op.HasStarted "a-2")
        op.Release("a-2", "R2")

        waitUntil (fun () -> received.Count = 2)
        CollectionAssert.AreEquivalent([ "R1"; "R2" ], received)

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``mapAsyncWithResultConverter applies the result converter before the strategy sees it``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let received = List<string>()
        let l = results |> listenS received.Add
        let strategy = AlwaysStartStrategy<unit, int>()

        let operation (v : string) (_ : CancellationToken) = Task.FromResult(v.ToUpperInvariant())

        let status =
            source
            |> mapAsyncWithResultConverter results errors operation strategy (fun (v : string) -> v.Length) None None true

        source |> sendS "hello"
        waitUntil (fun () -> received.Count = 1)

        CollectionAssert.AreEqual([ 5 ], strategy.CompletedResults)
        Assert.AreEqual("HELLO", received[0])

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``mapAsyncWithConverters applies both converters to strategy types unrelated to TInput or TResult``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let received = List<string>()
        let l = results |> listenS received.Add
        let strategy = AlwaysStartStrategy<int, bool>()

        let operation (v : string) (_ : CancellationToken) = Task.FromResult(v.ToUpperInvariant())

        // 'TStrategyInput (int, a length) and 'TStrategyResult (bool, "is long") are both unrelated
        // by inheritance to 'TInput/'TResult (string) — only this overload permits that.
        let status =
            source
            |> mapAsyncWithConverters
                results errors operation strategy
                (fun (v : string) -> v.Length)
                (fun (v : string) -> v.Length > 3)
                None None true

        source |> sendS "hello"
        waitUntil (fun () -> received.Count = 1)

        CollectionAssert.AreEqual([ 5 ], strategy.AdmittedValues)
        CollectionAssert.AreEqual([ true ], strategy.CompletedResults)
        Assert.AreEqual("HELLO", received[0])

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``a custom strategy using EmptyState works``() =
        let source = sinkS<string> ()
        let results = sinkS<unit> ()
        let errors = sinkS<exn> ()
        let received = List<unit>()
        let l = results |> listenS received.Add
        let strategy = CountingStrategy()

        let operation (_ : string) (_ : CancellationToken) = Task.FromResult(())

        let status =
            source |> mapAsync results errors operation strategy None None true

        source |> sendS "a"
        source |> sendS "b"

        waitUntil (fun () -> received.Count = 2)
        Assert.AreEqual(2, strategy.AdmittedCount)

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``cancelAll cancels every tracked operation``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let cancelAll = sinkS<unit> ()
        let op = ControlledOperation<string, string>()
        let received = List<string>()
        let l = results |> listenS received.Add

        let status =
            source
            |> mapAsync results errors op.Operation (parallelStrategy ()) (Some cancelAll) None true

        source |> sendS "a"
        source |> sendS "b"
        waitUntil (fun () -> op.HasStarted "a" && op.HasStarted "b")

        cancelAll |> sendS ()

        Thread.Sleep(200)
        Assert.AreEqual(0, received.Count, "A canceled outcome must never be published.")

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``cancelMatching cancels only tracked operations for matching input values``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let cancelMatching = sinkS<IReadOnlyCollection<string>> ()
        let op = ControlledOperation<string, string>()
        let received = List<string>()
        let l = results |> listenS received.Add

        let status =
            source
            |> mapAsync results errors op.Operation (parallelStrategy ()) None (Some cancelMatching) true

        source |> sendS "a"
        source |> sendS "b"
        waitUntil (fun () -> op.HasStarted "a" && op.HasStarted "b")

        cancelMatching |> sendS ([| "a" |] :> IReadOnlyCollection<string>)

        op.Release("b", "B")
        waitUntil (fun () -> received.Count = 1)

        Thread.Sleep(100)
        CollectionAssert.AreEqual([ "B" ], received)

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``cancelOnDispose true cancels an in-flight item``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let op = ControlledOperation<string, string>()
        let received = List<string>()
        let l = results |> listenS received.Add

        let status =
            source |> mapAsync results errors op.Operation (parallelStrategy ()) None None true

        source |> sendS "a"
        waitUntil (fun () -> op.HasStarted "a")

        status.Dispose()

        Thread.Sleep(200)
        Assert.AreEqual(0, received.Count)

        l |> unlistenL

    [<Test>]
    member _.``cancelOnDispose false lets an in-flight item finish and publish``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let op = ControlledOperation<string, string>()
        let received = List<string>()
        let l = results |> listenS received.Add

        let status =
            source |> mapAsync results errors op.Operation (parallelStrategy ()) None None false

        source |> sendS "a"
        waitUntil (fun () -> op.HasStarted "a")

        status.Dispose()
        op.Release("a", "A")

        waitUntil (fun () -> received.Count = 1)
        CollectionAssert.AreEqual([ "A" ], received)

        l |> unlistenL

    [<Test>]
    member _.``a failed operation publishes to errors``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let thrown = InvalidOperationException("boom")
        let received = List<exn>()
        let l = errors |> listenS received.Add

        let operation (_ : string) (_ : CancellationToken) : Task<string> = Task.FromException<string>(thrown)

        let status =
            source |> mapAsync results errors operation (parallelStrategy ()) None None true

        source |> sendS "hello"
        waitUntil (fun () -> received.Count = 1)
        Assert.AreSame(thrown, received[0])

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``Items and IsRunning reflect queued and running status``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let op = ControlledOperation<string, string>()

        let status =
            source |> mapAsync results errors op.Operation (queueStrategy ()) None None true

        Assert.IsFalse(status.IsRunning |> sampleC)
        Assert.AreEqual(0, (status.Items |> sampleC).Count)

        source |> sendS "a"
        source |> sendS "b"
        waitUntil (fun () -> op.HasStarted "a")
        waitUntil (fun () -> status.IsRunning |> sampleC)

        let items = status.Items |> sampleC
        Assert.AreEqual(2, items.Count)

        op.Release("a", "A")
        waitUntil (fun () -> op.HasStarted "b")
        op.Release("b", "B")

        waitUntil (fun () -> (status.Items |> sampleC).Count = 0)
        Assert.IsFalse(status.IsRunning |> sampleC)

        status.Dispose()
