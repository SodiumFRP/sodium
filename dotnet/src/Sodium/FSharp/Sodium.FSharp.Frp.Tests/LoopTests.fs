module Sodium.Frp.Tests.Loop

open System.Collections.Generic
open NUnit.Framework
open Sodium.Frp

[<TestFixture>]
type ``Loop Tests``() =

    [<Test>]
    member __.``Functional Stream Loop``() =
        let s = sinkS ()
        let result = loopWithNoCapturesS (fun r -> s |> snapshotC (r |> holdS 0) (+))
        let out = List<_>()
        let l = result |> listenS out.Add
        s |> sendS 1
        s |> sendS 2
        s |> sendS 3
        l |> unlistenL
        CollectionAssert.AreEqual([1;3;6], out)

    [<Test>]
    member __.``Functional Stream Loop With Captures``() =
        let s = sinkS ()
        let struct (result, s2) = loopS (fun r -> struct (s |> snapshotC (r |> holdS 0) (+), s |> mapS ((*) 2)))
        let out = List<_>()
        let out2 = List<_>()
        (
            use _l = result |> listenS out.Add
            use _l = s2 |> listenS out2.Add
            s |> sendS 1
            s |> sendS 2
            s |> sendS 3
        )
        CollectionAssert.AreEqual([1;3;6], out)
        CollectionAssert.AreEqual([2;4;6], out2)

    [<Test>]
    member __.``Functional Behavior Loop``() =
        let s = sinkB 0
        let result = loopWithNoCapturesB (fun r -> s |> Operational.updates |> snapshotB r (+) |> holdS 0 |> asBehaviorC)
        let out = List<_>()
        let l = runT (fun () -> result |> Operational.value |> listenS out.Add)
        s |> sendB 1
        s |> sendB 2
        s |> sendB 3
        l |> unlistenL
        CollectionAssert.AreEqual([0;1;3;6], out)

    [<Test>]
    member __.``Functional Behavior Loop With Captures``() =
        let s = sinkB 0
        let struct (result, s2) = loopB (fun r -> struct (s |> Operational.updates |> snapshotB r (+) |> holdS 0 |> asBehaviorC, s |> mapB ((*) 2)))
        let out = List<_>()
        let out2 = List<_>()
        (
            use _l = runT (fun () -> result |> Operational.value |> listenS out.Add)
            use _l = runT (fun () -> s2 |> Operational.value |> listenS out2.Add)
            s |> sendB 1
            s |> sendB 2
            s |> sendB 3
        )
        CollectionAssert.AreEqual([0;1;3;6], out)
        CollectionAssert.AreEqual([0;2;4;6], out2)

    [<Test>]
    member __.``Functional Cell Loop``() =
        let s = sinkC 0
        let result = loopWithNoCapturesC (fun r -> s |> updatesC |> snapshotC r (+) |> holdS 0)
        let out = List<_>()
        let l = runT (fun () -> result |> listenC out.Add)
        s |> sendC 1
        s |> sendC 2
        s |> sendC 3
        l |> unlistenL
        CollectionAssert.AreEqual([0;1;3;6], out)

    [<Test>]
    member __.``Functional Cell Loop With Captures``() =
        let s = sinkC 0
        let struct (result, s2) = loopC (fun r -> struct (s |> updatesC |> snapshotC r (+) |> holdS 0, s |> mapC ((*) 2)))
        let out = List<_>()
        let out2 = List<_>()
        (
            use _l = runT (fun () -> result |> listenC out.Add)
            use _l = s2 |> listenC out2.Add
            s |> sendC 1
            s |> sendC 2
            s |> sendC 3
        )
        CollectionAssert.AreEqual([0;1;3;6], out)
        CollectionAssert.AreEqual([0;2;4;6], out2)