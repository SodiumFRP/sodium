module Sodium.Frp.Tests.Behavior

open System
open System.Collections.Generic
open System.Threading.Tasks
open NUnit.Framework
open Sodium.Frp

type private TestObject (n1 : int, n2 : int) =
    let removeStreamSink = sinkS ()
    let changeNumber1StreamSink = sinkS ()
    let changeNumber2StreamSink = sinkS ()
    let number1Cell = changeNumber1StreamSink |> holdS n1
    let number2Cell = changeNumber2StreamSink |> holdS n2
    
    member val RemoveStream : Stream<unit> = upcast removeStreamSink
    member __.Number1Cell = number1Cell
    member __.Number2Cell = number2Cell
    member val BothNumbersCell = (number1Cell, number2Cell) |> lift2C (fun n1 n2 -> (n1, n2))
    
    member __.Remove () = removeStreamSink |> sendS ()
    member __.ChangeNumber1 n = changeNumber1StreamSink |> sendS n
    member __.ChangeNumber2 n = changeNumber2StreamSink |> sendS n

type private Sc = { a : char option; b : char option; sw : Cell<char> option }
type private Sc2 = { c : CellSink<int> }
type private Ss = { a : char; b : char; sw : Stream<char> option }
type private Ss2 = { s : StreamSink<int> }
type private Test (initialValue : int) = member val Value = sinkB initialValue

[<TestFixture>]
type ``Behavior Tests``() =

    [<Test>]
    member __.``Test Hold``() =
        let s = sinkS ()
        let c = s |> holdS 0
        let out = List<_>()
        let l = c |> listenC out.Add
        s |> sendS 2
        s |> sendS 9
        l |> unlistenL
        CollectionAssert.AreEqual([0;2;9], out)

    [<Test>]
    member __.``Test Send Null``() =
        let c = sinkC ""
        let out = List<_>()
        let l = c |> listenC out.Add
        c |> sendC "0"
        c |> sendC null
        c |> sendC "1"
        l |> unlistenL
        CollectionAssert.AreEqual(["";"0";null;"1"], out)

    [<Test>]
    member __.``Test Hold Updates``() =
        let s = sinkS ()
        let c = s |> holdS 0
        let out = List<_>()
        let l = c |> updatesC |> listenS out.Add
        s |> sendS 2
        s |> sendS 9
        l |> unlistenL
        CollectionAssert.AreEqual([2;9], out)

    [<Test>]
    member __.``Test Snapshot``() =
        let b = sinkB 0
        let trigger = sinkS ()
        let out = List<_>()
        let l = trigger |> snapshotB b (fun x y -> (string x) + " " + (string y)) |> listenS out.Add
        trigger |> sendS 100L
        b |> sendB 2
        trigger |> sendS 200L
        b |> sendB 9
        b |> sendB 1
        trigger |> sendS 300L
        l |> unlistenL
        CollectionAssert.AreEqual(["100 0";"200 2";"300 1"], out)
        
    [<Test>]
    member __.``Test Listen``() =
        let c = sinkC 9
        let out = List<_>()
        let l = c |> listenC out.Add
        c |> sendC 2
        c |> sendC 7
        l |> unlistenL
        CollectionAssert.AreEqual([9;2;7], out)

    [<Test>]
    member __.``Test ListenOnce``() =
        let c = sinkC 9
        let out = List<_>()
        let l = runT (fun () -> c |> valuesC |> listenOnceS out.Add)
        c |> sendC 2
        c |> sendC 7
        l |> unlistenL
        CollectionAssert.AreEqual([9], out)

    [<Test>]
    member __.``Test ListenOnce Updates``() =
        let c = sinkC 9
        let out = List<_>()
        let l = runT (fun () -> c |> updatesC |> listenOnceS out.Add)
        c |> sendC 2
        c |> sendC 7
        l |> unlistenL
        CollectionAssert.AreEqual([2], out)

    [<Test>]
    member __.``Test ListenOnceAsync``() =
        async {
            let c = sinkC 9
            let! result = runT (fun () -> c |> valuesC |> listenOnceAsyncS)
            c |> sendC 2
            c |> sendC 7
            Assert.AreEqual(9, result)
        } |> Async.StartAsTask :> Task

    [<Test>]
    member __.``Test Updates``() =
        let c = sinkC 9
        let out = List<_>()
        let l = c |> updatesC |> listenS out.Add
        c |> sendC 2
        c |> sendC 7
        CollectionAssert.AreEqual([2;7], out)

    [<Test>]
    member __.``Test Values``() =
        let c = sinkC 9
        let out = List<_>()
        let l = runT (fun () -> c |> valuesC |> listenS out.Add)
        c |> sendC 2
        c |> sendC 7
        CollectionAssert.AreEqual([9;2;7], out)

    [<Test>]
    member __.``Test CellLoop Complex``() =
        let s = sinkS ()
        let addItemStreamSink = sinkS ()
        let removeItemsStreamSink = sinkS ()
        let listCell = loopWithNoCapturesC (fun listCell ->
            (
                (addItemStreamSink, (s |> mapS (fun v -> v, v)))
                    |> orElseS
                    |> mapS (fun o -> (fun c -> Seq.append c [(TestObject o)] |> Array.ofSeq)),
                removeItemsStreamSink |> mapS (fun o -> (fun c -> c |> Seq.except o |> Array.ofSeq))
            )
                |> mergeS (fun f g -> (fun c -> f c |> g))
                |> snapshotC listCell (fun f c -> f c)
                |> holdS Array.empty)
        let l2 = runT (fun () ->
            listCell |> valuesC |> listenS (fun c -> postT (fun () ->
                if c.Length > 0 then
                    let (n1, n2) = (c |> Array.last).BothNumbersCell |> sampleC
                    if n1 = 9 && n2 = 9 then addItemStreamSink |> sendS (0, 0))))
        let yo = Seq.map (fun (o : TestObject) -> o.RemoveStream |> mapToS [|o|]) >> mergeAllS Array.append
        let l3 = runT (fun () -> listCell |> mapC (Seq.map (fun o -> o.RemoveStream |> mapToS [|o|]) >> mergeAllS Array.append) |> switchS |> listenS (fun o -> postT (fun () -> removeItemsStreamSink |> sendS o)))
        let l4 = runT (fun () -> listCell |> mapC (fun c -> if c.Length > 0 then ((c |> Array.last).Number1Cell, (c |> Array.last).Number2Cell) |> lift2C (fun x y -> x = 9 && y = 9) |> updatesC else neverS ()) |> switchS |> filterS id |> listenS (fun _ -> postT (fun () -> addItemStreamSink |> sendS (0, 0))))
        let out = List<_>()
        let l = listCell |> mapC (fun c -> c |> Seq.map (fun o -> (o.Number1Cell, o.Number2Cell) |> lift2C (fun x y -> x, y)) |> liftAllC id) |> switchC |> listenC out.Add
        addItemStreamSink |> sendS (5, 2)
        addItemStreamSink |> sendS (9, 2)
        (listCell |> sampleC |> (fun o -> o.[0])).Remove ()
        addItemStreamSink |> sendS (2, 9)
        (listCell |> sampleC |> (fun o -> o.[1])).ChangeNumber1 9
        addItemStreamSink |> sendS (9, 9)
        s |> sendS 5
        s |> sendS 9
        runT (fun () ->
            addItemStreamSink |> sendS (5, 5)
            s |> sendS 5)
        (listCell |> sampleC |> (fun o -> o.[8])).ChangeNumber2 9
        (listCell |> sampleC |> (fun o -> o.[8])).ChangeNumber1 9
        l |> unlistenL
        l2 |> unlistenL
        l3 |> unlistenL
        l4 |> unlistenL
        
        let expected =
            [|
                Array.empty;
                [|(5,2)|];
                [|(5,2);(9,2)|];
                [|(9,2)|];
                [|(9,2);(2,9)|];
                [|(9,2);(9,9)|];
                [|(9,2);(9,9);(0,0)|];
                [|(9,2);(9,9);(0,0);(9,9)|];
                [|(9,2);(9,9);(0,0);(9,9);(0,0)|];
                [|(9,2);(9,9);(0,0);(9,9);(0,0);(5,5)|];
                [|(9,2);(9,9);(0,0);(9,9);(0,0);(5,5);(9,9)|];
                [|(9,2);(9,9);(0,0);(9,9);(0,0);(5,5);(9,9);(0,0)|];
                [|(9,2);(9,9);(0,0);(9,9);(0,0);(5,5);(9,9);(0,0);(5,5)|];
                [|(9,2);(9,9);(0,0);(9,9);(0,0);(5,5);(9,9);(0,0);(5,9)|];
                [|(9,2);(9,9);(0,0);(9,9);(0,0);(5,5);(9,9);(0,0);(9,9)|];
                [|(9,2);(9,9);(0,0);(9,9);(0,0);(5,5);(9,9);(0,0);(9,9);(0,0)|]
            |]
        Assert.AreEqual(expected.Length, out.Count)
        expected |>
            Array.iteri (fun i e ->
                let o = out.[i]
                Assert.AreEqual(e.Length, o.Count)
                e |> Array.iteri (fun i e -> Assert.AreEqual(e, o.[i])))

    [<Test>]
    member __.``Test CellLoop``() =
        let s = sinkS ()
        let cell = loopWithNoCapturesC (fun cell -> s |> snapshotC cell (+) |> holdS 1)
        let out = List<_> ()
        let l = cell |> listenC out.Add
        s |> sendS 3
        s |> sendS 4
        s |> sendS 7
        s |> sendS 8
        l |> unlistenL
        CollectionAssert.AreEqual ([1;4;8;15;23], out)

    [<Test>]
    member __.``Test CellLoop Throws Exception``() =
        let actual =
            try
                let s = sinkS ()
                let cell = loopWithNoCapturesC (fun cell -> (cell |> updatesC |> filterS (fun v -> v % 2 = 0) |> mapS ((+) 1), s) |> mergeS (fun l r -> r) |> holdS 1)
                let out = List<_> ()
                let l = cell |> listenC out.Add
                s |> sendS 3
                s |> sendS 4
                s |> sendS 7
                s |> sendS 8
                l |> unlistenL
                None
            with
                | :? AggregateException as e ->
                    e.InnerExceptions |> Seq.tryFind (fun e -> e.Message = "A dependency cycle was detected.")
                | e -> Some e
        actual |> assertExceptionExists (fun e -> Assert.AreEqual ("A dependency cycle was detected.", e.Message))

    [<Test>]
    member __.``Test CellLoop SwitchS``() =
        let addStreamSink = sinkS ()
        let cell : Cell<TestObject []> = loopWithNoCapturesC (fun cell ->
            (
                cell
                    |> mapC
                        (Seq.map (fun o -> o.RemoveStream |> mapToS [|o|]) >> mergeAllS Array.append)
                            |> switchS
                            |> mapS Array.except,
                (addStreamSink |> mapS Array.singleton |> mapS Array.append)
            )
                |> mergeS (>>)
                |> snapshotC cell (<|)
                |> holdS Array.empty)
        let out = List<_> ()
        let l = cell |> listenC (out.Add << Array.length)
        let t1 = TestObject (1, 1)
        addStreamSink |> sendS t1
        let t2 = TestObject (2, 2)
        addStreamSink |> sendS t2
        let t3 = TestObject (3, 3)
        addStreamSink |> sendS t3
        t2.Remove ()
        let t4 = TestObject (4, 4)
        runT (fun () ->
            addStreamSink |> sendS t4
            t3.Remove ())
        let t5 = TestObject (5, 5)
        addStreamSink |> sendS t5
        l |> unlistenL
        CollectionAssert.AreEqual ([0;1;2;3;2;2;3], out)
    
    [<Test>]
    member __.``Test Cell Values``() =
        let c = sinkC 9
        let out = List<_>()
        let l = runT (fun () -> c |> valuesC |> listenS out.Add)
        c |> sendC 2
        c |> sendC 7
        l |> unlistenL
        CollectionAssert.AreEqual([9;2;7], out)
    
    [<Test>]
    member __.``Test Cell Values No Transaction``() =
        let c = sinkC 9
        let out = List<_>()
        let l = c |> valuesC |> listenS out.Add
        c |> sendC 2
        c |> sendC 7
        l |> unlistenL
        CollectionAssert.AreEqual([2;7], out)

    [<Test>]
    member __.``Test Value Then Map``() =
        let b = sinkB 9
        let out = List<_>()
        let l = runT (fun () -> b |> Operational.value |> mapS ((+) 100) |> listenS out.Add)
        b |> sendB 2
        b |> sendB 7
        l |> unlistenL
        CollectionAssert.AreEqual([109;102;107], out)

    [<Test>]
    member __.``Test Cell Values Then Map``() =
        let c = sinkC 9
        let out = List<_>()
        let l = runT (fun () -> c |> valuesC |> mapS ((+) 100) |> listenS out.Add)
        c |> sendC 2
        c |> sendC 7
        l |> unlistenL
        CollectionAssert.AreEqual([109;102;107], out)

    [<Test>]
    member __.``Test Value Then Merge``() =
        let b1 = sinkB 9
        let b2 = sinkB 2
        let out = List<_>()
        let l = runT (fun () -> (b1 |> Operational.value, b2 |> Operational.value) |> mergeS (+) |> listenS out.Add)
        b1 |> sendB 1
        b2 |> sendB 4
        runT (fun () ->
            b1 |> sendB 7
            b2 |> sendB 5)
        l |> unlistenL
        CollectionAssert.AreEqual([11;1;4;12], out)

    [<Test>]
    member __.``Test Cell Values Then Merge``() =
        let c1 = sinkC 9
        let c2 = sinkC 2
        let out = List<_>()
        let l = runT (fun () -> (c1 |> valuesC, c2 |> valuesC) |> mergeS (+) |> listenS out.Add)
        c1 |> sendC 1
        c2 |> sendC 4
        runT (fun () ->
            c1 |> sendC 7
            c2 |> sendC 5)
        l |> unlistenL
        CollectionAssert.AreEqual([11;1;4;12], out)

    [<Test>]
    member __.``Test Value Then Filter``() =
        let b = sinkB 9
        let out = List<_>()
        let l = runT (fun () -> b |> Operational.value |> filterS (fun x -> x % 2 <> 0) |> listenS out.Add)
        b |> sendB 2
        b |> sendB 7
        l |> unlistenL
        CollectionAssert.AreEqual([9;7], out)

    [<Test>]
    member __.``Test Cell Values Then Filter``() =
        let c = sinkC 9
        let out = List<_>()
        let l = runT (fun () -> c |> valuesC |> filterS (fun x -> x % 2 <> 0) |> listenS out.Add)
        c |> sendC 2
        c |> sendC 7
        l |> unlistenL
        CollectionAssert.AreEqual([9;7], out)

    [<Test>]
    member __.``Test Value Then Once``() =
        let b = sinkB 9
        let out = List<_>()
        let l = runT (fun () -> b |> Operational.value |> onceS |> listenS out.Add)
        b |> sendB 2
        b |> sendB 7
        l |> unlistenL
        CollectionAssert.AreEqual([9], out)

    [<Test>]
    member __.``Test Cell Values Then Once``() =
        let c = sinkC 9
        let out = List<_>()
        let l = runT (fun () -> c |> valuesC |> onceS |> listenS out.Add)
        c |> sendC 2
        c |> sendC 7
        l |> unlistenL
        CollectionAssert.AreEqual([9], out)

    [<Test>]
    member __.``Test Value Then Late Listen``() =
        let b = sinkB 9
        let out = List<_>()
        let value = b |> Operational.value
        b |> sendB 8
        let l = value |> listenS out.Add
        b |> sendB 2
        b |> sendB 7
        l |> unlistenL
        CollectionAssert.AreEqual([2;7], out)

    [<Test>]
    member __.``Test Cell Values Then Late Listen``() =
        let c = sinkC 9
        let out = List<_>()
        let value = c |> valuesC
        c |> sendC 8
        let l = value |> listenS out.Add
        c |> sendC 2
        c |> sendC 7
        l |> unlistenL
        CollectionAssert.AreEqual([2;7], out)

    [<Test>]
    member __.``Test Map``() =
        let c = sinkC 6
        let out = List<_>()
        let l = c |> mapC string |> listenC out.Add
        c |> sendC 8
        l |> unlistenL
        CollectionAssert.AreEqual(["6";"8"], out)

    [<Test>]
    member __.``Test Map Late Listen``() =
        let c = sinkC 6
        let out = List<_>()
        let cm = c |> mapC string
        c |> sendC 2
        let l = cm |> listenC out.Add
        c |> sendC 8
        l |> unlistenL
        CollectionAssert.AreEqual(["2";"8"], out)

    [<Test>]
    member __.``Test Calm``() =
        let c = sinkC 2
        let out = List<_>()
        let l = c |> calmC |> listenC out.Add
        c |> sendC 2
        c |> sendC 2
        c |> sendC 4
        c |> sendC 2
        c |> sendC 4
        c |> sendC 4
        c |> sendC 2
        c |> sendC 2
        l |> unlistenL
        CollectionAssert.AreEqual([2;4;2;4;2], out)

    [<Test>]
    member __.``Test Calm 2``() =
        let c = sinkC 2
        let out = List<_>()
        let l = c |> calmC |> listenC out.Add
        c |> sendC 4
        c |> sendC 2
        c |> sendC 4
        c |> sendC 4
        c |> sendC 2
        c |> sendC 2
        l |> unlistenL
        CollectionAssert.AreEqual([2;4;2;4;2], out)

    [<Test>]
    member __.``Test Apply``() =
        let cf = sinkC (fun x -> "1 " + string x)
        let ca = sinkC 5L
        let out = List<_>()
        let l = ca |> applyC cf |> listenC out.Add
        cf |> sendC (fun x -> "12 " + string x)
        ca |> sendC 6L
        l |> unlistenL
        CollectionAssert.AreEqual(["1 5";"12 5";"12 6"], out)

    [<Test>]
    member __.``Test Lift``() =
        let c1 = sinkC 1
        let c2 = sinkC 5L
        let out = List<_>()
        let l = (c1, c2) |> lift2C (fun x y -> string x + " " + string y) |> listenC out.Add
        c1 |> sendC 12
        c2 |> sendC 6L
        l |> unlistenL
        CollectionAssert.AreEqual(["1 5";"12 5";"12 6"], out)

    [<Test>]
    member __.``Test Lift Glitch``() =
        let c1 = sinkC 1
        let c3 = c1 |> mapC ((*) 3)
        let c5 = c1 |> mapC ((*) 5)
        let c = (c3, c5) |> lift2C (fun x y -> string x + " " + string y)
        let out = List<_>()
        let l = c |> listenC out.Add
        c1 |> sendC 2
        l |> unlistenL
        CollectionAssert.AreEqual(["3 5";"6 10"], out)

    [<Test>]
    member __.``Test Lift From Simultaneous``() =
        let (c1, c2) = runT (fun () ->
            let c1 = sinkC 3
            let c2 = sinkC 5
            c2 |> sendC 7
            (c1, c2))
        let out = List<_>()
        let l = (c1, c2) |> lift2C (+) |> listenC out.Add
        l |> unlistenL
        CollectionAssert.AreEqual([10], out)

    [<Test>]
    member __.``Test Hold Is Delayed``() =
        let s = sinkS ()
        let h = s |> holdS 0
        let pair = s |> snapshotC h (fun a b -> string a + " " + string b)
        let out = List<_>()
        let l = pair |> listenS out.Add
        s |> sendS 2
        s |> sendS 3
        l |> unlistenL
        CollectionAssert.AreEqual(["2 0";"3 2"], out)

    [<Test>]
    member __.``Test SwitchC``() =
        let ssc = sinkS<Sc> ()
        let ca = ssc |> mapS (fun s -> s.a) |> filterOptionS |> holdS 'A'
        let cb = ssc |> mapS (fun s -> s.b) |> filterOptionS |> holdS 'a'
        let csw = ssc |> mapS (fun s -> s.sw) |> filterOptionS |> holdS ca
        let co = csw |> switchC
        let out = List<_>()
        let l = co |> listenC out.Add
        ssc |> sendS { a = Option.Some 'B'; b = Option.Some 'b'; sw = Option.None }
        ssc |> sendS { a = Option.Some 'C'; b = Option.Some 'c'; sw = Option.Some cb }
        ssc |> sendS { a = Option.Some 'D'; b = Option.Some 'd'; sw = Option.None }
        ssc |> sendS { a = Option.Some 'E'; b = Option.Some 'e'; sw = Option.Some ca }
        ssc |> sendS { a = Option.Some 'F'; b = Option.Some 'f'; sw = Option.None }
        ssc |> sendS { a = Option.None; b = Option.None; sw = Option.Some cb }
        ssc |> sendS { a = Option.None; b = Option.None; sw = Option.Some ca }
        ssc |> sendS { a = Option.Some 'G'; b = Option.Some 'g'; sw = Option.Some cb }
        ssc |> sendS { a = Option.Some 'H'; b = Option.Some 'h'; sw = Option.Some ca }
        ssc |> sendS { a = Option.Some 'I'; b = Option.Some 'i'; sw = Option.Some ca }
        l |> unlistenL
        CollectionAssert.AreEqual(['A';'B';'c';'d';'E';'F';'f';'F';'g';'H';'I'], out)

    [<Test>]
    member __.``Test SwitchC Simultaneous``() =
        let sc1 = { c = sinkC 0 }
        let csc = sinkC sc1
        let co = csc |> mapC (fun b -> b.c) |> switchC
        let out = List<_>()
        let l = co |> listenC out.Add
        let sc2 = { c = sinkC 3 }
        let sc3 = { c = sinkC 4 }
        let sc4 = { c = sinkC 7 }
        sc1.c |> sendC 1
        sc1.c |> sendC 2
        csc |> sendC sc2
        sc1.c |> sendC 3
        sc2.c |> sendC 4
        sc3.c |> sendC 5
        csc |> sendC sc3
        sc3.c |> sendC 6
        sc3.c |> sendC 7
        runT (fun () ->
            sc3.c |> sendC 2
            csc |> sendC sc4
            sc4.c |> sendC 8)
        sc4.c |> sendC 9
        l |> unlistenL
        CollectionAssert.AreEqual([0;1;2;3;4;5;6;7;8;9], out)

    [<Test>]
    member __.``Test SwitchS``() =
        let sss = sinkS ()
        let sa = sss |> mapS (fun s -> s.a)
        let sb = sss |> mapS (fun s -> s.b)
        let csw = sss |> mapS (fun s -> s.sw) |> filterOptionS |> holdS sa
        let so = csw |> switchS
        let out = List<_>()
        let l = so |> listenS out.Add
        sss |> sendS { a = 'A'; b = 'a'; sw = Option.None }
        sss |> sendS { a = 'B'; b = 'b'; sw = Option.None }
        sss |> sendS { a = 'C'; b = 'c'; sw = Option.Some sb }
        sss |> sendS { a = 'D'; b = 'd'; sw = Option.None }
        sss |> sendS { a = 'E'; b = 'e'; sw = Option.Some sa }
        sss |> sendS { a = 'F'; b = 'f'; sw = Option.None }
        sss |> sendS { a = 'G'; b = 'g'; sw = Option.Some sb }
        sss |> sendS { a = 'H'; b = 'h'; sw = Option.Some sa }
        sss |> sendS { a = 'I'; b = 'i'; sw = Option.Some sa }
        l |> unlistenL
        CollectionAssert.AreEqual(['A';'B';'C';'d';'e';'F';'G';'h';'I'], out)

    [<Test>]
    member __.``Test SwitchS Simultaneous``() =
        let ss1 = { s = sinkS () }
        let css = sinkB ss1
        let so = css |> mapB (fun b -> b.s) |> switchSB
        let out = List<_>()
        let l = so |> listenS out.Add
        let ss2 = { s = sinkS () }
        let ss3 = { s = sinkS () }
        let ss4 = { s = sinkS () }
        ss1.s |> sendS 0
        ss1.s |> sendS 1
        ss1.s |> sendS 2
        css |> sendB ss2
        ss1.s |> sendS 7
        ss2.s |> sendS 3
        ss2.s |> sendS 4
        ss3.s |> sendS 2
        css |> sendB ss3
        ss3.s |> sendS 5
        ss3.s |> sendS 6
        ss3.s |> sendS 7
        runT (fun () ->
            ss3.s |> sendS 8
            css |> sendB ss4
            ss4.s |> sendS 2)
        ss4.s |> sendS 9
        l |> unlistenL
        CollectionAssert.AreEqual([0;1;2;3;4;5;6;7;8;9], out)

    [<Test>]
    member __.``Test Lift List``() =
        let cellSinks = List.init 50 (fun _ -> sinkC 1)
        let sum = cellSinks |> liftAllC Seq.sum
        let out = List<_>()
        (
            use _l = sum |> listenC out.Add
            cellSinks.[4] |> sendC 5
            cellSinks.[5] |> sendC 5
            runT (fun () ->
                cellSinks.[9] |> sendC 5
                cellSinks.[17] |> sendC 5
                cellSinks.[41] |> sendC 5
                cellSinks.[48] |> sendC 5)
        )
        CollectionAssert.AreEqual([50;54;58;74], out)

    [<Test>]
    member __.``Test Lift Loop List``() =
        let struct (c, s) = runT (fun () ->
            let struct (_, (c, s)) = loopC (fun c1 ->
                let s1 = sinkC 1
                let struct (_, (c, s)) = loopC (fun c2 ->
                    let s2 = sinkC 1
                    let struct (_, (c, s)) = loopC (fun c3 ->
                        let s3 = sinkC 1
                        let struct (_, (c, s)) = loopC (fun c4 ->
                            let s4 = sinkC 1
                            let struct (_, (c, s)) = loopC (fun c5 ->
                                let s5 = sinkC 1
                                struct (s5, ([|c1;c2;c3;c4;c5|] |> liftAllC Seq.sum, s5)))
                            struct (s4, (c, [|s;s4|])))
                        struct (s3, (c, s |> Array.append [|s3|])))
                    struct (s2, (c, s |> Array.append [|s2|])))
                struct (s1, (c, s |> Array.append [|s1|])))
            struct (c, s))
        let out = List<_>()
        let l = c |> listenC out.Add
        s.[2] |> sendC 5
        s.[3] |> sendC 5
        runT (fun () ->
            s.[1] |> sendC 5
            s.[4] |> sendC 5)
        l |> unlistenL
        CollectionAssert.AreEqual([5;9;13;21], out)

    [<Test>]
    member __.``Test Lift List Large``() =
        let cellSinks = List.init 500 (fun _ -> sinkC 1)
        let sum = cellSinks |> liftAllC Seq.sum
        let out = List<_>()
        let l = sum |> listenC out.Add
        cellSinks.[4] |> sendC 5
        cellSinks.[5] |> sendC 5
        runT (fun () ->
            cellSinks.[9] |> sendC 5
            cellSinks.[17] |> sendC 5
            cellSinks.[41] |> sendC 5
            cellSinks.[48] |> sendC 5)
        l |> unlistenL
        CollectionAssert.AreEqual([500;504;508;524], out)

    [<Test>]
    member __.``Test Lift List Large Many Updates``() =
        let cellSinks = List.init 500 (fun _ -> sinkC 1)
        let sum = cellSinks |> liftAllC Seq.sum
        let out = List<_>()
        let l = sum |> listenC out.Add
        for i = 0 to 99 do
            cellSinks.[i * 5] |> sendC 5
            cellSinks.[i * 5 + 1] |> sendC 5
            runT (fun () ->
                cellSinks.[i * 5 + 2] |> sendC 5
                cellSinks.[i * 5 + 3] |> sendC 5
                cellSinks.[i * 5 + 4] |> sendC 5)
        l |> unlistenL
        let expected = List.Cons (500, List.concat (List.init 100 (fun i -> [500 + 20 * i + 4; 500 + 20 * i + 8; 500 + 20 * i + 20])))
        CollectionAssert.AreEqual(expected, out)

    [<Test>]
    member __.``Test Lift Changes While Listening``() =
        let cellSinks = List.init 50 (fun _ -> sinkC 1)
        let sum = cellSinks |> liftAllC Seq.sum
        let out = List<_>()
        let l = runT (fun () ->
            cellSinks.[4] |> sendC 5
            let l = sum |> listenC out.Add
            cellSinks.[5] |> sendC 5
            l)
        cellSinks.[9] |> sendC 5
        runT (fun () ->
            cellSinks.[17] |> sendC 5
            cellSinks.[41] |> sendC 5
            cellSinks.[48] |> sendC 5)
        l.Unlisten ()
        CollectionAssert.AreEqual([58;62;74], out)

    [<Test>]
    member __.``SwitchC On CellLoop``() =
        let struct (_, (c, c1, c2, s)) = loopC (fun cl ->
            let c1 = sinkC 1
            let c2 = sinkC 11
            let c = cl |> switchC
            let s = sinkC (c1 :> Cell<_>)
            struct (s, (c, c1, c2, s)))
        let out = List<_>()
        let l = c |> listenC out.Add
        c1 |> sendC 2
        c2 |> sendC 12
        runT (fun () ->
            c1 |> sendC 3
            c2 |> sendC 13
            s |> sendC (upcast c2))
        c1 |> sendC 4
        c2 |> sendC 14
        l |> unlistenL
        CollectionAssert.AreEqual([1;2;13;14], out)

    [<Test>]
    member __.``SwitchS On BehaviorLoop``() =
        let struct (_, (b, b1, b2, s)) = loopB (fun bl ->
            let b1 = sinkS<int> ()
            let b2 = sinkS<int> ()
            let b = bl |> switchSB
            let s = sinkB (b1 :> Stream<_>)
            struct (s, (b, b1, b2, s)))
        let out = List<_>()
        let l = b |> listenS out.Add
        b1 |> sendS 2
        b2 |> sendS 12
        runT (fun () ->
            b1 |> sendS 3
            b2 |> sendS 13
            s |> sendB (upcast b2))
        b1 |> sendS 4
        b2 |> sendS 14
        l |> unlistenL
        CollectionAssert.AreEqual([2;3;14], out)

    [<Test>]
    member __.``SwitchC Catch First``() =
        let out = List<_>()
        let (c1, c2, s, l) = runT (fun () ->
            let c1 = sinkC 1
            let c2 = sinkC 11
            let s = sinkB (c1 :> Cell<_>)
            let c = s |> switchCB
            c1 |> sendC 2
            c2 |> sendC 12
            s |> sendB (upcast c2)
            let l = c |> listenC out.Add
            (c1, c2, s, l))
        c1 |> sendC 3
        c2 |> sendC 13
        runT (fun () ->
            c1 |> sendC 4
            c2 |> sendC 14
            s |> sendB (upcast c1))
        c1 |> sendC 5
        c2 |> sendC 15
        l |> unlistenL
        CollectionAssert.AreEqual([12;13;4;5], out)

    [<Test>]
    member __.``SwitchS Catch First``() =
        let out = List<_>()
        let (c1, c2, s, l) = runT (fun () ->
            let c1 = sinkS<int> ()
            let c2 = sinkS<int> ()
            let s = sinkB (c1 :> Stream<_>)
            let c = s |> switchSB
            c1 |> sendS 2
            c2 |> sendS 12
            s |> sendB (upcast c2)
            let l = c |> listenS out.Add
            (c1, c2, s, l))
        c1 |> sendS 3
        c2 |> sendS 13
        runT (fun () ->
            c1 |> sendS 4
            c2 |> sendS 14
            s |> sendB (upcast c1))
        c1 |> sendS 5
        c2 |> sendS 15
        l |> unlistenL
        CollectionAssert.AreEqual([2;13;14;5], out)

    [<Test>]
    member __.``SwitchS Catch First Before``() =
        let out = List<_>()
        let (c1, c2, s, l) = runT (fun () ->
            let c1 = sinkS<int> ()
            let c2 = sinkS<int> ()
            let s = sinkB (c1 :> Stream<_>)
            c1 |> sendS 2
            c2 |> sendS 12
            s |> sendB (upcast c2)
            let c = s |> switchSB
            let l = c |> listenS out.Add
            (c1, c2, s, l))
        c1 |> sendS 3
        c2 |> sendS 13
        runT (fun () ->
            c1 |> sendS 4
            c2 |> sendS 14
            s |> sendB (upcast c1))
        c1 |> sendS 5
        c2 |> sendS 15
        l |> unlistenL
        CollectionAssert.AreEqual([2;13;14;5], out)

    [<Test>]
    member __.``Test Lift In SwitchC``() =
        let list1 = [|Test(0);Test(1);Test(2);Test(3);Test(4)|]
        let list2 = [|Test(5);Test(6);Test(7);Test(8);Test(9)|]
        let v = sinkB list1
        let c = v |> mapB ((Seq.map (fun o -> o.Value)) >> liftAllB id) |> switchBB
        let streamOutput = List<_>()
        let l = c |> Operational.updates |> listenS streamOutput.Add
        let cellOutput = List<_>()
        let l2 = runT (fun () -> c |> Operational.value |> listenS cellOutput.Add)
        list1.[2].Value |> sendB 12
        list2.[1].Value |> sendB 16
        list1.[4].Value |> sendB 14
        runT (fun () ->
            list2.[2].Value |> sendB 17
            list1.[0].Value |> sendB 10
            v |> sendB list2)
        list1.[3].Value |> sendB 13
        list2.[3].Value |> sendB 18
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
        let v = sinkB list1
        let c = v |> mapB ((Seq.map (fun o -> o.Value)) >> liftAllB id) |> mapB id |> switchBB
        let streamOutput = List<_>()
        let l = c |> Operational.updates |> listenS streamOutput.Add
        let cellOutput = List<_>()
        let l2 = runT (fun () -> c |> Operational.value |> listenS cellOutput.Add)
        list1.[2].Value |> sendB 12
        list2.[1].Value |> sendB 16
        list1.[4].Value |> sendB 14
        runT (fun () ->
            list2.[2].Value |> sendB 17
            list1.[0].Value |> sendB 10
            v |> sendB list2)
        list1.[3].Value |> sendB 13
        list2.[3].Value |> sendB 18
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