module Sodium.Frp.Async.Tests.MapAsyncTests

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open Sodium.Frp
open Sodium.Frp.Async
open Sodium.Frp.Async.Tests.TestUtil

// A hand-written custom strategy (subclassing AsyncConcurrencyStrategy<'TInput,'TResult,'TState>
// and overriding Admit/OnCompleted/CreateState) is deliberately NOT exercised in this file: F#
// cannot construct AsyncToStart<'T> or AsyncStrategyResult<'T> — both protected-internal nested
// types declared in the Core.Frp.Async assembly — from an override in a foreign assembly. It's a
// genuine F# compiler limitation, confirmed by trying both the implicit and explicit `new` call
// forms; both are rejected as inaccessible even though the identical construction compiles fine in
// C#, and even though naming/reading these same types (e.g. incoming.Value, outside a closure)
// works once qualified through AsyncMapBase. Every built-in strategy the F# wrapper exposes is
// therefore implemented in C# (AsyncConcurrencyStrategyFactory) rather than in this module. The
// tests below stay within what an F# consumer can actually build today: the five built-in strategy
// functions, used through all four mapAsync variants.

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
    member _.``mapAsyncWithResultConverter accepts a strategy that erases the result to unit``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let received = List<string>()
        let l = results |> listenS received.Add

        let operation (v : string) (_ : CancellationToken) = Task.FromResult(v.ToUpperInvariant())

        // parallelStrategy() : AsyncConcurrencyStrategyBase<unit, unit> fits this overload's
        // AsyncConcurrencyStrategyBase<unit, 'TStrategyResult> shape exactly at 'TStrategyResult =
        // unit, so a resultConverter that discards the real TResult is enough to use it here — the
        // real result (untouched by that converter) is still what reaches `results`.
        let status =
            source
            |> mapAsyncWithResultConverter results errors operation (parallelStrategy ()) (fun (_ : string) -> ()) None None true

        source |> sendS "hello"
        waitUntil (fun () -> received.Count = 1)
        Assert.AreEqual("HELLO", received.[0])

        status.Dispose()
        l |> unlistenL

    [<Test>]
    member _.``mapAsyncWithConverters applies an input converter to a strategy typed against a custom group key``() =
        let source = sinkS<string> ()
        let results = sinkS<string> ()
        let errors = sinkS<exn> ()
        let op = ControlledOperation<string, string>()
        let received = List<string>()
        let l = results |> listenS received.Add

        // queuePerGroupStrategy's TStrategyInput (here, an int length) is unrelated by inheritance
        // to TInput (string), and its TStrategyResult is unit — the fully general overload is what
        // lets both an inputConverter and a (discarding) resultConverter be supplied at once.
        let strategy = queuePerGroupStrategy (fun (len : int) -> len % 2)

        let status =
            source
            |> mapAsyncWithConverters
                results errors op.Operation strategy
                (fun (v : string) -> v.Length)
                (fun (_ : string) -> ())
                None None true

        // "hi" (length 2) and "bob" (length 3) land in different groups, so both start at once.
        source |> sendS "hi"
        source |> sendS "bob"
        waitUntil (fun () -> op.HasStarted "hi" && op.HasStarted "bob")

        op.Release("hi", "HI")
        op.Release("bob", "BOB")
        waitUntil (fun () -> received.Count = 2)

        CollectionAssert.AreEquivalent([ "HI"; "BOB" ], received)

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
        Assert.AreSame(thrown, received.[0])

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
