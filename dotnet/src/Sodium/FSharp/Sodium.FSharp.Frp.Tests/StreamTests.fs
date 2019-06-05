module Sodium.Frp.Tests.Stream

open System
open System.Collections.Generic
open NUnit.Framework
open Sodium.Frp
open System.Threading

[<TestFixture>]
type ``Stream Tests``() =

    [<Test>]
    member __.``Test Stream Send``() =
        let s = sinkS ()
        let out = List<_>()
        let l = s |> listenS out.Add
        s |> sendS 5
        l |> unlistenL
        CollectionAssert.AreEqual([5], out)
        s |> sendS 6
        CollectionAssert.AreEqual([5], out)

    [<Test>]
    member __.``Test Stream Send In Callback Throws Exception``() =
        let s = sinkS ()
        let s2 = sinkS ()
        let actual = (
            use _let = s |> listenS (s2 |> flip sendS)
            try
                s |> sendS 5
                None
            with
                | :? InvalidOperationException as e -> Some e
        )
        actual |> assertExceptionExists (fun e -> Assert.AreEqual ("Send may not be called inside a Sodium callback.", e.Message))

    [<Test>]
    member __.``Test Stream Send In Map Throws Exception``() =
        let s = sinkS ()
        let s2 = sinkS ()
        let actual = (
            use _let = s |> mapS (s2 |> flip sendS) |> listenS id
            try
                s |> sendS 5
                None
            with
                | :? InvalidOperationException as e -> Some e
        )
        actual |> assertExceptionExists (fun e -> Assert.AreEqual ("Send may not be called inside a Sodium callback.", e.Message))

    [<Test>]
    member __.``Test Stream Send In Cell Map Throws Exception``() =
        let c = constantC 5
        let s2 = sinkS ()
        let actual = (
            try
                use _let = c |> mapC (s2 |> flip sendS) |> listenC id
                None
            with
                | :? InvalidOperationException as e -> Some e
        )
        actual |> assertExceptionExists (fun e -> Assert.AreEqual ("Send may not be called inside a Sodium callback.", e.Message))

    [<Test>]
    member __.``Test Stream Send In Cell Lift Throws Exception``() =
        let c = constantC 5
        let c2 = constantC 7
        let s2 = sinkS ()
        let actual = (
            try
                use _let = (c, c2) |> lift2C (fun _ _ -> s2 |> sendS 5) |> listenC id
                None
            with
                | :? InvalidOperationException as e -> Some e
        )
        actual |> assertExceptionExists (fun e -> Assert.AreEqual ("Send may not be called inside a Sodium callback.", e.Message))

    [<Test>]
    member __.``Test Stream Send In Cell Apply Throws Exception``() =
        let c = constantC 5
        let s2 = sinkS ()
        let c2 = constantC (fun _ -> s2 |> sendS 5)
        let actual = (
            try
                use _let = c |> applyC c2 |> listenC id
                None
            with
                | :? InvalidOperationException as e -> Some e
        )
        actual |> assertExceptionExists (fun e -> Assert.AreEqual ("Send may not be called inside a Sodium callback.", e.Message))

    [<Test>]
    member __.``Test Map``() =
        let s = sinkS ()
        let m = s |> mapS ((+) 2 >> string)
        let out = List<_>()
        let l = m |> listenS out.Add
        s |> sendS 5
        s |> sendS 3
        l |> unlistenL
        CollectionAssert.AreEqual(["7";"5"], out)

    [<Test>]
    member __.``Test OrElse Non-Simultaneous``() =
        let s1 = sinkS ()
        let s2 = sinkS ()
        let out = List<_>()
        let l = (s1, s2) |> orElseS |> listenS out.Add
        s1 |> sendS 7
        s1 |> sendS 9
        s1 |> sendS 8
        l |> unlistenL
        CollectionAssert.AreEqual([7;9;8], out)

    [<Test>]
    member __.``Test OrElse Simultaneous 1``() =
        let s1 = sinkWithCoalesceS (fun l r -> r)
        let s2 = sinkWithCoalesceS (fun l r -> r)
        let out = List<_>()
        let l = (s2, s1) |> orElseS |> listenS out.Add
        runT (fun () -> s1 |> sendS 7; s2 |> sendS 60)
        runT (fun () -> s1 |> sendS 9)
        runT (fun () -> s1 |> sendS 7; s1 |> sendS 60; s2 |> sendS 8; s2 |> sendS 90)
        runT (fun () -> s2 |> sendS 8; s2 |> sendS 90; s1 |> sendS 7; s1 |> sendS 60)
        runT (fun () -> s2 |> sendS 8; s1 |> sendS 7; s2 |> sendS 90; s1 |> sendS 60)
        l |> unlistenL
        CollectionAssert.AreEqual([60;9;90;90;90], out)

    [<Test>]
    member __.``Test OrElse Simultaneous 2``() =
        let s = sinkS ()
        let s2 = s |> mapS ((*) 2)
        let out = List<_>()
        let l = (s, s2) |> orElseS |> listenS out.Add
        s |> sendS 7
        s |> sendS 9
        l |> unlistenL
        CollectionAssert.AreEqual([7;9], out)

    [<Test>]
    member __.``Test OrElse Left Bias``() =
        let s = sinkS ()
        let s2 = s |> mapS ((*) 2)
        let out = List<_>()
        let l = (s2, s) |> orElseS |> listenS out.Add
        s |> sendS 7
        s |> sendS 9
        l |> unlistenL
        CollectionAssert.AreEqual([14;18], out)

    [<Test>]
    member __.``Test Merge Non-Simultaneous``() =
        let s1 = sinkS ()
        let s2 = sinkS ()
        let out = List<_>()
        let l = (s1, s2) |> mergeS (+) |> listenS out.Add
        s1 |> sendS 7
        s1 |> sendS 9
        s1 |> sendS 8
        l |> unlistenL
        CollectionAssert.AreEqual([7;9;8], out)

    [<Test>]
    member __.``Test Merge Simultaneous``() =
        let s = sinkS ()
        let s2 = s |> mapS ((*) 2)
        let out = List<_>()
        let l = (s, s2) |> mergeS (+) |> listenS out.Add
        s |> sendS 7
        s |> sendS 9
        l |> unlistenL
        CollectionAssert.AreEqual([21;27], out)

    [<Test>]
    member __.``Test Coalesce``() =
        let s = sinkWithCoalesceS (+)
        let out = List<_>()
        let l = s |> listenS out.Add
        runT (fun () -> s |> sendS 2)
        runT (fun () -> s |> sendS 8; s |> sendS 40)
        l |> unlistenL
        CollectionAssert.AreEqual([2;48], out)

    [<Test>]
    member __.``Test Coalesce 2``() =
        let s = sinkWithCoalesceS (+)
        let out = List<_>()
        let l = s |> listenS out.Add
        runT (fun () -> Seq.init 5 ((+) 1) |> Seq.iter (s |> flip sendS))
        runT (fun () -> Seq.init 5 ((+) 6) |> Seq.iter (s |> flip sendS))
        l |> unlistenL
        CollectionAssert.AreEqual([15;40], out)

    [<Test>]
    member __.``Test Filter``() =
        let s = sinkS ()
        let out = List<_>()
        let l = s |> filterS Char.IsUpper |> listenS out.Add
        s |> sendS 'H'
        s |> sendS 'o'
        s |> sendS 'I'
        l |> unlistenL
        CollectionAssert.AreEqual (['H';'I'], out)

    [<Test>]
    member __.``Test Filter Option``() =
        let s = sinkS ()
        let out = List<_>()
        let l = s |> filterOptionS |> listenS out.Add
        s |> sendS (Some "tomato")
        s |> sendS None
        s |> sendS (Some "peach")
        s |> sendS None
        s |> sendS (Some "pear")
        l |> unlistenL
        CollectionAssert.AreEqual (["tomato";"peach";"pear"], out)

    [<Test>]
    member __.``Test Loop Stream``() =
        let sa = sinkS ()
        let struct (sb, sc) = loopS (fun sb ->
            let sc = (sa |> mapS (flip (%) 10), sb) |> mergeS (*)
            let sb = sa |> mapS (flip (/) 10) |> filterS ((<>) 0)
            struct (sb, sc))
        let out = List<_>()
        let out2 = List<_>()
        let l = sb |> listenS out.Add
        let l2 = sc |> listenS out2.Add
        sa |> sendS 2
        sa |> sendS 52
        l2 |> unlistenL
        l |> unlistenL
        CollectionAssert.AreEqual ([5], out)
        CollectionAssert.AreEqual ([2;10], out2)

    [<Test>]
    member __.``Test Loop Cell``() =
        let ca = sinkC 22
        let struct (cb, cc) = loopC (fun cb ->
            let cc = (ca |> mapC (flip (%) 10), cb) |> lift2C (*)
            let cb = ca |> mapC (flip (/) 10)
            struct (cb, cc))
        let out = List<_>()
        let out2 = List<_>()
        let l = cb |> listenC out.Add
        let l2 = cc |> listenC out2.Add
        ca |> sendC 2
        ca |> sendC 52
        l2 |> unlistenL
        l |> unlistenL
        CollectionAssert.AreEqual ([2;0;5], out)
        CollectionAssert.AreEqual ([4;0;10], out2)

    [<Test>]
    member __.``Test Gate``() =
        let sc = sinkS ()
        let cGate = sinkB true
        let out = List<_>()
        let l = sc |> gateB cGate |> listenS out.Add
        sc |> sendS 'H'
        cGate |> sendB false
        sc |> sendS 'O'
        cGate |> sendB true
        sc |> sendS 'I'
        l |> unlistenL
        CollectionAssert.AreEqual (['H';'I'], out)
    
    [<Test>]
    member __.``Test Calm``() =
        let s = sinkS ()
        let out = List<_>()
        let l = s |> calmS |> listenS out.Add
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 4
        s |> sendS 2
        s |> sendS 4
        s |> sendS 4
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 4
        s |> sendS 2
        s |> sendS 4
        s |> sendS 4
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 4
        s |> sendS 2
        s |> sendS 4
        s |> sendS 4
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 4
        s |> sendS 2
        s |> sendS 4
        s |> sendS 4
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 2
        s |> sendS 4
        s |> sendS 2
        s |> sendS 4
        s |> sendS 4
        s |> sendS 2
        s |> sendS 2
        l |> unlistenL
        CollectionAssert.AreEqual ([2;4;2;4;2;4;2;4;2;4;2;4;2;4;2;4;2;4;2;4;2], out)
    
    [<Test>]
    member __.``Test Calm 2``() =
        let s = sinkS ()
        let out = List<_>()
        let l = s |> calmS |> listenS out.Add
        s |> sendS 2
        s |> sendS 4
        s |> sendS 2
        s |> sendS 4
        s |> sendS 4
        s |> sendS 2
        s |> sendS 2
        l |> unlistenL
        CollectionAssert.AreEqual ([2;4;2;4;2], out)
    
    [<Test>]
    member __.``Test Collect``() =
        let sa = sinkS ()
        let out = List<_>()
        let sum = sa |> collectS struct (100, true) (fun a struct (value, test) ->
            let outputValue = value + if test then a * 3 else a
            struct (outputValue, struct (outputValue, outputValue % 2 = 0)))
        let l = sum |> listenS out.Add
        sa |> sendS 5
        sa |> sendS 7
        sa |> sendS 1
        sa |> sendS 2
        sa |> sendS 3
        l |> unlistenL
        CollectionAssert.AreEqual ([115;122;125;127;130], out)
    
    [<Test>]
    member __.``Test Accum``() =
        let sa = sinkS ()
        let out = List<_>()
        let sum = sa |> accumS 100 (+)
        let l = sum |> listenC out.Add
        sa |> sendS 5
        sa |> sendS 7
        sa |> sendS 1
        sa |> sendS 2
        sa |> sendS 3
        l |> unlistenL
        CollectionAssert.AreEqual ([100;105;112;113;115;118], out)
    
    [<Test>]
    member __.``Test Once``() =
        let s = sinkS ()
        let out = List<_>()
        let l = s |> onceS |> listenS out.Add
        s |> sendS 'A'
        s |> sendS 'B'
        s |> sendS 'C'
        l |> unlistenL
        CollectionAssert.AreEqual (['A'], out)
    
    [<Test>]
    member __.``Test Hold``() =
        let s = sinkS ()
        let c = s |> holdS ' '
        let out = List<_>()
        let l = c |> listenC out.Add
        s |> sendS 'C'
        s |> sendS 'B'
        s |> sendS 'A'
        l |> unlistenL
        CollectionAssert.AreEqual ([' ';'C';'B';'A'], out)
    
    [<Test>]
    member __.``Test Hold Implicit Delay``() =
        let s = sinkS ()
        let c = s |> holdS ' '
        let out = List<_>()
        let l = s |> snapshotAndTakeC c |> listenS out.Add
        s |> sendS 'C'
        s |> sendS 'B'
        s |> sendS 'A'
        l |> unlistenL
        CollectionAssert.AreEqual ([' ';'C';'B'], out)
    
    [<Test>]
    member __.``Test Defer``() =
        let s = sinkS ()
        let c = s |> holdS ' '
        let out = List<_>()
        let l = s |> Operational.defer |> snapshotAndTakeC c |> listenS out.Add
        s |> sendS 'C'
        s |> sendS 'B'
        s |> sendS 'A'
        l |> unlistenL
        CollectionAssert.AreEqual (['C';'B';'A'], out)
    
    [<Test>]
    member __.``Test ListenWeak``() =
        let s = sinkS ()
        let out = List<_>()
        let a () =
            let l = s |> listenWeakS out.Add
            s |> sendS 1
            s |> sendS 2
        a ()
        GC.Collect (0, GCCollectionMode.Forced)
        GC.Collect (0, GCCollectionMode.Forced)
        s |> sendS 3
        s |> sendS 4
        Assert.AreEqual (2, out.Count)
    
    [<Test>]
    member __.``Test ListenWeak With Map``() =
        let s = sinkS ()
        let out = List<_>()
        let a () =
            let s2 = s |> mapS ((+) 1)
            let a () =
                let l = s |> listenWeakS out.Add
                s |> sendS 1
                s |> sendS 2
            a ()
            GC.Collect (0, GCCollectionMode.Forced)
            GC.Collect (0, GCCollectionMode.Forced)
            let a () =
                let l = s2 |> listenWeakS out.Add
                s |> sendS 3
                s |> sendS 4
                s |> sendS 5
            a ()
        a ()
        GC.Collect (0, GCCollectionMode.Forced)
        GC.Collect (0, GCCollectionMode.Forced)
        s |> sendS 6
        s |> sendS 7
        Assert.AreEqual (5, out.Count)
    
    [<Test>]
    member __.``Test Unlisten``() =
        let s = sinkS ()
        let out = List<_>()
        let a () =
            let l = s |> listenS out.Add
            s |> sendS 1
            l |> unlistenL
            s |> sendS 2
        a ()
        s |> sendS 3
        s |> sendS 4
        Assert.AreEqual (1, out.Count)
    
    [<Test>]
    member __.``Test Unlisten Weak``() =
        let s = sinkS ()
        let out = List<_>()
        let a () =
            let l = s |> listenWeakS out.Add
            s |> sendS 1
            l |> unlistenL
            s |> sendS 2
        a ()
        s |> sendS 3
        s |> sendS 4
        Assert.AreEqual (1, out.Count)
    
    [<Test>]
    member __.``Test Multiple Unlisten``() =
        let s = sinkS ()
        let out = List<_>()
        let a () =
            let l = s |> listenS out.Add
            s |> sendS 1
            l |> unlistenL
            l |> unlistenL
            s |> sendS 2
            l |> unlistenL
        a ()
        s |> sendS 3
        s |> sendS 4
        Assert.AreEqual (1, out.Count)
    
    [<Test>]
    member __.``Test Multiple Unlisten Weak``() =
        let s = sinkS ()
        let out = List<_>()
        let a () =
            let l = s |> listenWeakS out.Add
            s |> sendS 1
            l |> unlistenL
            l |> unlistenL
            s |> sendS 2
            l |> unlistenL
        a ()
        s |> sendS 3
        s |> sendS 4
        Assert.AreEqual (1, out.Count)
    
    [<Test>]
    member __.``Test ListenOnce``() =
        let s = sinkS ()
        let out = List<_>()
        let l = s |> listenOnceS out.Add
        s |> sendS 'A'
        s |> sendS 'B'
        s |> sendS 'C'
        l |> unlistenL
        CollectionAssert.AreEqual (['A'], out)
    
    [<Test>]
    member __.``Test ListenOnceAsync``() =
        async {
            let s = sinkS ()
            (Thread (fun () ->
                Thread.Sleep 250
                s |> sendS 'A'
                s |> sendS 'B'
                s |> sendS 'C')).Start ()
            let! r = s |> listenOnceAsyncS
            Assert.AreEqual ('A', r)
        } |> Async.StartAsVoidTask

    [<Test>]
    member __.``Test ListenOnceAsync Same Thread``() =
        async {
            let s = sinkS ()
            let r' = s |> listenOnceAsyncS
            s |> sendS 'A'
            s |> sendS 'B'
            s |> sendS 'C'
            let! r = r'
            Assert.AreEqual ('A', r)
        } |> Async.StartAsVoidTask
    
    [<Test>]
    member __.``Test Listen Async``() =
        async {
            let a = sinkC 1
            let a1 = a |> mapC ((+) 1)
            let a2 = a |> mapC ((*) 2)
            let struct (called, struct (results, l)) = loopC (fun calledLoop ->
                let result = (a1, a2) |> lift2C (+)
                let incrementStream = result |> valuesC |> mapToS ()
                let decrementStream = sinkS ()
                let called = (incrementStream |> mapToS 1, decrementStream |> mapToS -1) |> mergeS (+) |> snapshotC calledLoop (+) |> holdS 0
                let results = List<_>()
                let l = result |> listenC (fun v ->
                    async {
                        do! Async.Sleep 900
                        results.Add v
                        decrementStream |> sendS ()
                    } |> Async.Start)
                struct (called, struct (results, l)))
            let calledResults = List<_>()
            let l2 = called |> listenC calledResults.Add
            do! Async.Sleep 500
            a |> sendC 2
            do! Async.Sleep 500
            a |> sendC 3
            do! Async.Sleep 2500
            l2 |> unlistenL
            l |> unlistenL
        } |> Async.StartAsVoidTask
    
    [<Test>]
    member __.``Test Stream Loop``() =
        let streamSink = sinkS ()
        let s = loopWithNoCapturesS (fun sl ->
            let c = sl |> mapS ((+) 2) |> holdS 0
            streamSink |> snapshotC c (+))
        let out = List<_>()
        let l = s |> listenS out.Add
        streamSink |> sendS 3
        streamSink |> sendS 4
        streamSink |> sendS 7
        streamSink |> sendS 8
        l |> unlistenL
        CollectionAssert.AreEqual ([3;9;18;28], out)
    
    [<Test>]
    member __.``Test Stream Loop Defer``() =
        let streamSink = sinkS ()
        let stream = loopWithNoCapturesS (fun streamLoop ->
            (streamSink, streamLoop) |> orElseS |> filterS (flip (<) 5) |> mapS ((+) 1) |> Operational.defer)
        let out = List<_>()
        let l = stream |> listenS out.Add
        streamSink |> sendS 2
        l |> unlistenL
        CollectionAssert.AreEqual ([3;4;5], out)