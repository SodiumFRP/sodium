module Sodium.Frp.Tests.Cell

open System
open System.Collections.Generic
open NUnit.Framework
open Sodium.Frp

[<AutoOpen>]
module private Types =
    type Test (initialValue : int) = member val Value = sinkC initialValue
    type Inner = private { s' : StreamSink<int>; c' : Cell<int> }
        with
            static member create initialValue =
                let s = sinkS ()
                { s' = s; c' = s |> holdLazyS initialValue }
            member this.s = this.s'
            member this.c = this.c'

[<TestFixture>]
type ``Cell Tests``() =

    [<Test>]
    member __.``Test Loop``() =
        let struct (_, (c, s)) = loopC (fun cl ->
            let c = cl |> mapC ((*) 5)
            let s = sinkCS<int> ()
            struct (s |> holdS 3, (c, s)))
        let output1 = List<_>()
        let output2 = List<_>()
        let l = c |> listenC output1.Add
        let l2 = c |> updatesC |> listenS output2.Add
        s |> sendS 5
        s |> sendS 7
        l2 |> unlistenL
        l |> unlistenL
        CollectionAssert.AreEqual([15;25;35], output1)
        CollectionAssert.AreEqual([25;35], output2)

    [<Test>]
    member __.``Test Lift Simultaneous Updates``() =
        let out = List<_>()
        let cellSink = sinkC 1
        let cell = cellSink |> mapC ((*) 2)
        let l = (cellSink, cell) |> lift2C (+) |> updatesC |> listenS out.Add
        cellSink |> sendC 2
        cellSink |> sendC 7
        l |> unlistenL
        CollectionAssert.AreEqual([6;21],out)

    [<Test>]
    member __.``Test Lift In SwitchC``() =
        let list1 = [|Test(0);Test(1);Test(2);Test(3);Test(4)|]
        let list2 = [|Test(5);Test(6);Test(7);Test(8);Test(9)|]
        let v = sinkC list1
        let c = v |> mapC (Seq.map (fun o -> o.Value) >> liftAllC id) |> switchC
        let streamOutput = List<_>()
        let l = c |> updatesC |> listenS streamOutput.Add
        let cellOutput = List<_>()
        let l2 = c |> listenC cellOutput.Add
        list1.[2].Value |> sendC 12
        list2.[1].Value |> sendC 16
        list1.[4].Value |> sendC 14
        runT (fun () ->
            list2.[2].Value |> sendC 17
            list1.[0].Value |> sendC 10
            v |> sendC list2)
        list1.[3].Value |> sendC 13
        list2.[3].Value |> sendC 18
        l2 |> unlistenL
        l |> unlistenL
        Assert.AreEqual (4, streamOutput.Count)
        Assert.AreEqual (5, cellOutput.Count)
        CollectionAssert.AreEqual ([0;1;2;3;4], cellOutput.[0])
        CollectionAssert.AreEqual ([0;1;12;3;4], streamOutput.[0])
        CollectionAssert.AreEqual ([0;1;12;3;4], cellOutput.[1])
        CollectionAssert.AreEqual ([0;1;12;3;14], streamOutput.[1])
        CollectionAssert.AreEqual ([0;1;12;3;14], cellOutput.[2])
        CollectionAssert.AreEqual ([5;16;17;8;9], streamOutput.[2])
        CollectionAssert.AreEqual ([5;16;17;8;9], cellOutput.[3])
        CollectionAssert.AreEqual ([5;16;17;18;9], streamOutput.[3])
        CollectionAssert.AreEqual ([5;16;17;18;9], cellOutput.[4])

    [<Test>]
    member __.``Test Map With SwitchC``() =
        let list1 = [|Test(0);Test(1);Test(2);Test(3);Test(4)|]
        let list2 = [|Test(5);Test(6);Test(7);Test(8);Test(9)|]
        let v = sinkC list1
        let c = v |> mapC ((Seq.map (fun o -> o.Value)) >> liftAllC id) |> mapC id |> switchC
        let streamOutput = List<_>()
        let l = c |> updatesC |> listenS streamOutput.Add
        let cellOutput = List<_>()
        let l2 = c |> listenC cellOutput.Add
        list1.[2].Value |> sendC 12
        list2.[1].Value |> sendC 16
        list1.[4].Value |> sendC 14
        runT (fun () ->
            list2.[2].Value |> sendC 17
            list1.[0].Value |> sendC 10
            v |> sendC list2)
        list1.[3].Value |> sendC 13
        list2.[3].Value |> sendC 18
        l2 |> unlistenL
        l |> unlistenL
        Assert.AreEqual (4, streamOutput.Count)
        Assert.AreEqual (5, cellOutput.Count)
        CollectionAssert.AreEqual ([0;1;2;3;4], cellOutput.[0])
        CollectionAssert.AreEqual ([0;1;12;3;4], streamOutput.[0])
        CollectionAssert.AreEqual ([0;1;12;3;4], cellOutput.[1])
        CollectionAssert.AreEqual ([0;1;12;3;14], streamOutput.[1])
        CollectionAssert.AreEqual ([0;1;12;3;14], cellOutput.[2])
        CollectionAssert.AreEqual ([5;16;17;8;9], streamOutput.[2])
        CollectionAssert.AreEqual ([5;16;17;8;9], cellOutput.[3])
        CollectionAssert.AreEqual ([5;16;17;18;9], streamOutput.[3])
        CollectionAssert.AreEqual ([5;16;17;18;9], cellOutput.[4])

    [<Test>]
    member __.``Test Lift Cells In SwitchC``() =
        let out = List<_>()
        let s = sinkC 0
        let c = constantC <| constantC 1
        let r = c |> mapC (fun c -> (c, s) |> lift2C (+))
        let l = r |> switchC |> listenC out.Add
        s |> sendC 2
        s |> sendC 4
        l |> unlistenL

    [<Test>]
    member __.``Test Lazy Cell Creation``() =
        let out = List<_>()
        let s = sinkS ()
        let c = constantC 1 |> mapC (fun _ -> s |> holdS 0)
        s |> sendS 1
        let l = c |> switchC |> listenC out.Add
        s |> sendS 3
        s |> sendS 5
        l |> unlistenL
        CollectionAssert.AreEqual([1;3;5],out)

    [<Test>]
    member __.``Test Cell Values With Previous``() =
        let s = sinkS ()
        let c = s |> holdS 0
        let out = List<_>()
        let l = runT (fun () ->
            let r = (c |> updatesC |> snapshotC c (fun c p -> (c, Some p)), constantLazyC (c |> sampleLazyC) |> valuesC |> mapS (fun c -> (c, None))) |> orElseS
            r |> listenS out.Add)
        s |> sendS 1
        s |> sendS 2
        s |> sendS 3
        s |> sendS 4
        l |> unlistenL
        CollectionAssert.AreEqual([(0,None);(1,Some 0);(2,Some 1);(3,Some 2);(4,Some 3)],out)

    [<Test>]
    member __.``Test Cell Values With Previous Having Initial Update``() =
        let s = sinkS ()
        let c = s |> holdS 0
        let out = List<_>()
        let l = runT (fun () ->
            let r = (c |> updatesC |> snapshotC c (fun c p -> (c, Some p)), constantLazyC (c |> sampleLazyC) |> valuesC |> mapS (fun c -> (c, None))) |> orElseS
            s |> sendS 1
            r |> listenS out.Add)
        s |> sendS 2
        s |> sendS 3
        s |> sendS 4
        s |> sendS 5
        l |> unlistenL
        CollectionAssert.AreEqual([(1,Some 0);(2,Some 1);(3,Some 2);(4,Some 3);(5,Some 4)],out)

    [<Test>]
    member __.``Test Loop And SwitchC Error``() =
        let e =
            try
                loopWithNoCapturesC (fun c ->
                    let s = sinkS ()
                    let cc = s |> holdS (Inner.create (c |> sampleLazyC))
                    cc |> mapC (fun o -> o.c) |> switchC) |> ignore
                None
            with
                | :? InvalidOperationException as e -> Some e
        e |> assertExceptionExists (fun e -> Assert.AreEqual ("ValueFactory attempted to access the Value property of this instance.", e.Message))

    [<Test>]
    member __.``Test Loop And SwitchC``() =
        let struct (resultCell, (innerCell, innerStreamSink)) =
            loopC (fun c ->
                let s = sinkS ()
                let cc = s |> holdS (Inner.create (c |> sampleLazyC))
                struct (cc |> mapC (fun o -> o.c) |> switchC |> valuesC |> holdS 3, (cc, s)))
        let out = List<_>()
        (
            use _l = resultCell |> listenC out.Add
            (innerCell |> sampleC).s |> sendS 5
            innerStreamSink |> sendS (Inner.create (resultCell |> sampleLazyC |> Lazy.map (fun v -> v - 1)))
            (innerCell |> sampleC).s |> sendS 7
        )
        CollectionAssert.AreEqual([3;5;4;7],out)