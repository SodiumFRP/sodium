module Sodium.Frp.Tests.Builder

open System.Collections.Generic
open NUnit.Framework
open Sodium.Frp

[<TestFixture>]
type ``Builder Tests``() =
    [<Test>]
    member _.``Test Stream Map``() =
        let trigger = sinkS ()
        let out = List<_>()
        let l = transform { for trigger in trigger do select (string trigger) } |> listenS out.Add
        trigger |> sendS 1L
        trigger |> sendS 2L
        trigger |> sendS 3L
        trigger |> sendS 4L
        trigger |> sendS 5L
        l |> unlistenL
        CollectionAssert.AreEqual(["1";"2";"3";"4";"5"], out)
        
    [<Test>]
    member _.``Test Where``() =
        let trigger = sinkS ()
        let out = List<_>()
        let l =
            transform {
                for trigger in trigger do
                where (trigger % 2L = 0L)
                select (string trigger)
            } |> listenS out.Add
        trigger |> sendS 1L
        trigger |> sendS 2L
        trigger |> sendS 3L
        trigger |> sendS 4L
        trigger |> sendS 5L
        l |> unlistenL
        CollectionAssert.AreEqual(["2";"4"], out)
        
    [<Test>]
    member _.``Test Where 2``() =
        let c = sinkC 0
        let trigger = sinkS ()
        let out = List<_>()
        let l =
            let input =
                snapshot {
                    let! trigger = trigger
                    and! c = c
                    return trigger + (int64 c)
                }
            
            let output =
                transform {
                    for input in input do
                    select (input, input % 2L = 0L) into (input, isEven)
                    where isEven
                    select (string input) into s
                    select (s + "!")
                }
            
            output |> listenS out.Add
        trigger |> sendS 1L
        trigger |> sendS 2L
        trigger |> sendS 3L
        trigger |> sendS 4L
        trigger |> sendS 5L
        l |> unlistenL
        CollectionAssert.AreEqual(["2!";"4!"], out)
        
    [<Test>]
    member _.``Test Where Some``() =
        let trigger = sinkS ()
        let out = List<_>()
        let l =
            transform {
                for trigger in trigger do
                whereSome into trigger
                select (string trigger)
            } |> listenS out.Add
        trigger |> sendS (Some 1L)
        trigger |> sendS None
        trigger |> sendS (Some 2L)
        trigger |> sendS (Some 3L)
        trigger |> sendS None
        trigger |> sendS (Some 4L)
        trigger |> sendS (Some 5L)
        l |> unlistenL
        CollectionAssert.AreEqual(["1";"2";"3";"4";"5"], out)
        
    [<Test>]
    member _.``Test Where Some 2``() =
        let trigger = sinkS ()
        let out = List<_>()
        let l =
            transform {
                for trigger in trigger do
                select (trigger |> Option.map string)
                whereSome
            } |> listenS out.Add
        trigger |> sendS (Some 1L)
        trigger |> sendS None
        trigger |> sendS (Some 2L)
        trigger |> sendS (Some 3L)
        trigger |> sendS None
        trigger |> sendS (Some 4L)
        trigger |> sendS (Some 5L)
        l |> unlistenL
        CollectionAssert.AreEqual(["1";"2";"3";"4";"5"], out)
    
    [<Test>]
    member _.``Test Hold``() =
        let s = sinkS ()
        let c = transform { for s in s do hold ' ' }
        let out = List<_>()
        let l = c |> listenC out.Add
        s |> sendS 'C'
        s |> sendS 'B'
        s |> sendS 'A'
        l |> unlistenL
        CollectionAssert.AreEqual ([' ';'C';'B';'A'], out)
    
    [<Test>]
    member _.``Test OrElse``() =
        let s1 = sinkS ()
        let s2 = sinkS ()
        let s =
            transform {
                for v in s1 do
                select (v * 2)
                orElse
                    (transform {
                        for v in s2 do
                        select (v * 2)
                    })
            }
        let out = List<_>()
        let l = s |> listenS out.Add
        s1 |> sendS 2
        s2 |> sendS 3
        runT (fun () ->
            s1 |> sendS 2
            s2 |> sendS 3)
        l |> unlistenL
        CollectionAssert.AreEqual ([4;6;4], out)
    
    [<Test>]
    member _.``Test Stream Calm``() =
        let trigger = sinkS ()
        let out = List<_>()
        let l =
            transform {
                for trigger in trigger do
                calm
                select (string trigger)
            } |> listenS out.Add
        trigger |> sendS 1L
        trigger |> sendS 1L
        trigger |> sendS 2L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        l |> unlistenL
        CollectionAssert.AreEqual(["1";"2";"3"], out)
    
    [<Test>]
    member _.``Test Stream Calm 2``() =
        let trigger = sinkS ()
        let out = List<_>()
        let l =
            transform {
                for trigger in trigger do
                select (string trigger)
                calm
            } |> listenS out.Add
        trigger |> sendS 1L
        trigger |> sendS 1L
        trigger |> sendS 2L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        l |> unlistenL
        CollectionAssert.AreEqual(["1";"2";"3"], out)
    
    [<Test>]
    member _.``Test Stream Calm With Comparer``() =
        let trigger = sinkS ()
        let out = List<_>()
        let l =
            transform {
                for trigger in trigger do
                calm (fun x y -> abs(x - y) < 2L)
                select (string trigger)
            } |> listenS out.Add
        trigger |> sendS 1L
        trigger |> sendS 1L
        trigger |> sendS 2L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        l |> unlistenL
        CollectionAssert.AreEqual(["1";"3"], out)
    
    [<Test>]
    member _.``Test Stream Calm With Comparer 2``() =
        let trigger = sinkS ()
        let out = List<_>()
        let l =
            transform {
                for trigger in trigger do
                select (string trigger)
                calm (fun x y -> abs(int x - int y) < 2)
            } |> listenS out.Add
        trigger |> sendS 1L
        trigger |> sendS 1L
        trigger |> sendS 2L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        l |> unlistenL
        CollectionAssert.AreEqual(["1";"3"], out)
    
    [<Test>]
    member _.``Test Stream Calm With Equality Comparer``() =
        let trigger = sinkS ()
        let out = List<_>()
        let comparer =
            { new IEqualityComparer<_> with
                member _.Equals(x,y) = abs(x - y) < 2L
                member _.GetHashCode x = 0 }
        let l =
            transform {
                for trigger in trigger do
                calm comparer
                select (string trigger)
            } |> listenS out.Add
        trigger |> sendS 1L
        trigger |> sendS 1L
        trigger |> sendS 2L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        l |> unlistenL
        CollectionAssert.AreEqual(["1";"3"], out)
    
    [<Test>]
    member _.``Test Stream Calm With Equality Comparer 2``() =
        let trigger = sinkS ()
        let out = List<_>()
        let comparer =
            { new IEqualityComparer<_> with
                member _.Equals(x,y) = abs(int x - int y) < 2
                member _.GetHashCode x = 0 }
        let l =
            transform {
                for trigger in trigger do
                select (string trigger)
                calm comparer
            } |> listenS out.Add
        trigger |> sendS 1L
        trigger |> sendS 1L
        trigger |> sendS 2L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        trigger |> sendS 3L
        l |> unlistenL
        CollectionAssert.AreEqual(["1";"3"], out)
        
    [<Test>]
    member _.``Test Merge``() =
        let s1 = sinkS ()
        let s2 = sinkS ()
        let s =
            merge {
                let! s1 = s1
                and! s2 = s2
                return (s1 + s2)
            }
        let out = List<_>()
        let l = s |> listenS out.Add
        s1 |> sendS 2
        s2 |> sendS 3
        runT (fun () ->
            s1 |> sendS 2
            s2 |> sendS 3)
        l |> unlistenL
        CollectionAssert.AreEqual ([2;3;5], out)
        
    [<Test>]
    member _.``Test Merge All``() =
        let streamSinks = Seq.init 5 (fun _ -> sinkS()) |> Seq.toList
        let out = List<_>()
        let l =
            mergeAll {
                let! l,r = streamSinks
                return l + r
            } |> listenS out.Add
        streamSinks[1] |> sendS 12
        streamSinks[3] |> sendS 6
        runT (fun () ->
            streamSinks[0] |> sendS 3
            streamSinks[2] |> sendS 9
            streamSinks[4] |> sendS 15)
        l |> unlistenL
        CollectionAssert.AreEqual([12;6;27], out)
    
    [<Test>]
    member _.``Test Cell Map``() =
        let c = sinkC 6
        let out = List<_>()
        let l = transform { for c in c do select (string c) } |> listenC out.Add
        c |> sendC 8
        l |> unlistenL
        CollectionAssert.AreEqual(["6";"8"], out)

    [<Test>]
    member _.``Test SwitchC``() =
        let c1 = sinkC 6
        let c2 = sinkC 8
        let c = sinkC (c1 |> asC)
        let out = List<_>()
        let l =
            transform {
                for c in c do
                switch into v
                select (2*v)
            } |> listenC out.Add
        c1 |> sendC 7
        c2 |> sendC 8
        c |> sendC c2
        c1 |> sendC 8
        c2 |> sendC 9
        l |> unlistenL
        CollectionAssert.AreEqual([12;14;16;18], out)

    [<Test>]
    member _.``Test SwitchC 2``() =
        let c1 = sinkC 6
        let c2 = sinkC 8
        let c = sinkC c1
        let out = List<_>()
        let l =
            transform {
                for c in c do
                switchC into v
                select (2*v)
            } |> listenC out.Add
        c1 |> sendC 7
        c2 |> sendC 8
        c |> sendC c2
        c1 |> sendC 8
        c2 |> sendC 9
        l |> unlistenL
        CollectionAssert.AreEqual([12;14;16;18], out)

    [<Test>]
    member _.``Test SwitchS``() =
        let s1 = sinkS ()
        let s2 = sinkS ()
        let c = sinkC (s1 |> asS)
        let out = List<_>()
        let l =
            transform {
                for c in c do
                switch into v
                where (v%2 = 0)
                select (2*v)
            } |> listenS out.Add
        s1 |> sendS 6
        s2 |> sendS 7
        s1 |> sendS 8
        c |> sendC s2
        s1 |> sendS 7
        s2 |> sendS 8
        s2 |> sendS 9
        l |> unlistenL
        CollectionAssert.AreEqual([12;16;16], out)

    [<Test>]
    member _.``Test SwitchS 2``() =
        let s1 = sinkS ()
        let s2 = sinkS ()
        let c = sinkC s1
        let out = List<_>()
        let l =
            transform {
                for c in c do
                switchS into v
                where (v%2 = 0)
                select (2*v)
            } |> listenS out.Add
        s1 |> sendS 6
        s2 |> sendS 7
        s1 |> sendS 8
        c |> sendC s2
        s1 |> sendS 7
        s2 |> sendS 8
        s2 |> sendS 9
        l |> unlistenL
        CollectionAssert.AreEqual([12;16;16], out)

    [<Test>]
    member _.``Test Lift Map``() =
        let c = sinkC 6
        let out = List<_>()
        let output = lift { let! c = c in return string c }
        let l = output |> listenC out.Add
        c |> sendC 8
        l |> unlistenL
        CollectionAssert.AreEqual(["6";"8"], out)
        
    [<Test>]
    member _.``Test Snapshot``() =
        let c = sinkC 0
        let trigger = sinkS ()
        let out = List<_>()
        let l =
            snapshot {
                let! trigger = trigger
                and! c = c
                return string trigger + " " + string c
            } |> listenS out.Add
        trigger |> sendS 100L
        c |> sendC 2
        trigger |> sendS 200L
        c |> sendC 9
        c |> sendC 1
        trigger |> sendS 300L
        l |> unlistenL
        CollectionAssert.AreEqual(["100 0";"200 2";"300 1"], out)
        
    [<Test>]
    member _.``Test Snapshot 11``() =
        let c = sinkC 0
        let trigger = sinkS ()
        let out = List<_>()
        let l =
            snapshot {
                let! trigger = trigger
                and! c1 = c
                and! c2 = c
                and! c3 = c
                and! c4 = c
                and! c5 = c
                and! c6 = c
                and! c7 = c
                and! c8 = c
                and! c9 = c
                and! c10 = c
                and! c11 = c
                return string trigger + " " + string c1 + " " + string c2 + " " + string c3 + " " + string c4 + " " + string c5 + " " + string c6 + " " + string c7 + " " + string c8 + " " + string c9 + " " + string c10 + " " + string c11
            } |> listenS out.Add
        trigger |> sendS 100L
        c |> sendC 2
        trigger |> sendS 200L
        c |> sendC 9
        c |> sendC 1
        trigger |> sendS 300L
        l |> unlistenL
        CollectionAssert.AreEqual(["100 0 0 0 0 0 0 0 0 0 0 0";"200 2 2 2 2 2 2 2 2 2 2 2";"300 1 1 1 1 1 1 1 1 1 1 1"], out)
        
    [<Test>]
    member _.``Test Lift 2``() =
        let c1 = sinkC 1
        let c2 = sinkC 5L
        let out = List<_>()
        let l =
            lift {
                let! c1 = c1
                and! c2 = c2
                return string c1 + " " + string c2
            } |> listenC out.Add
        c1 |> sendC 12
        c2 |> sendC 6L
        l |> unlistenL
        CollectionAssert.AreEqual(["1 5";"12 5";"12 6"], out)

    [<Test>]
    member _.``Test Lift 12``() =
        let c1 = sinkC 1
        let c2 = sinkC 5L
        let out = List<_>()
        let l =
            lift {
                let! c1 = c1
                and! c2 = c2
                and! c3 = c2
                and! c4 = c2
                and! c5 = c2
                and! c6 = c2
                and! c7 = c2
                and! c8 = c2
                and! c9 = c2
                and! c10 = c2
                and! c11 = c2
                and! c12 = c2
                return string c1 + " " + string c2 + " " + string c3 + " " + string c4 + " " + string c5 + " " + string c6 + " " + string c7 + " " + string c8 + " " + string c9 + " " + string c10 + " " + string c11 + " " + string c12
            } |> listenC out.Add
        c1 |> sendC 12
        c2 |> sendC 6L
        l |> unlistenL
        CollectionAssert.AreEqual(["1 5 5 5 5 5 5 5 5 5 5 5";"12 5 5 5 5 5 5 5 5 5 5 5";"12 6 6 6 6 6 6 6 6 6 6 6"], out)
    
    
    [<Test>]
    member _.``Test Lift All``() =
        let cellSinks = Seq.init 5 sinkC |> Seq.toList
        let out = List<_>()
        let l =
            liftAll {
                let! values = cellSinks
                return values |> Seq.map ((*) 2) |> Seq.toList
            } |> listenC out.Add
        cellSinks[1] |> sendC 12
        cellSinks[3] |> sendC 6
        l |> unlistenL
        CollectionAssert.AreEqual([[0;2;4;6;8];[0;24;4;6;8];[0;24;4;12;8]], out)