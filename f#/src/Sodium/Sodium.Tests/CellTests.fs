module Sodium.Tests.Cell

open NUnit.Framework
open Sodium
open System.Collections.Generic

type private Sc = { a : char option; b : char option; sw : char Cell option }
type private Sc2 = { c : int CellSink }
type private Ss = { a : char; b : char; sw : char Stream option }
type private Ss2 = { s : int StreamSink }

[<TestFixture>]
type Tests() =

    [<Test>]
    member __.``Test Hold``() =
        use s = Stream.sink ()
        use c = s |> Stream.hold 0
        let out = List<_>()
        (
            use _l = c |> Cell.listen out.Add
            s.Send 2
            s.Send 9
        )
        CollectionAssert.AreEqual([0;2;9], out)

    [<Test>]
    member __.``Test Hold Updates``() =
        use s = Stream.sink ()
        use c = s |> Stream.hold 0
        let out = List<_>()
        (
            use _l = c |> Operational.updates |> Stream.listen out.Add
            s.Send 2
            s.Send 9
        )
        CollectionAssert.AreEqual([2;9], out)

    [<Test>]
    member __.``Test Snapshot``() =
        use c = Cell.sink (0)
        use trigger = Stream.sink ()
        let out = List<_>()
        (
            use _l = trigger |> Stream.snapshot (fun x y -> (string x) + " " + (string y)) c |> Stream.listen out.Add
            trigger.Send 100L
            c.Send 2
            trigger.Send 200L
            c.Send 9
            c.Send 1
            trigger.Send 300L
        )
        CollectionAssert.AreEqual(["100 0";"200 2";"300 1"], out)

    [<Test>]
    member __.``Test Listen``() =
        use c = Cell.sink (9)
        let out = List<_>()
        (
            use _l = c |> Cell.listen out.Add
            c.Send 2
            c.Send 7
        )
        CollectionAssert.AreEqual([9;2;7], out)

    [<Test>]
    member __.``Test ListenOnce``() =
        use c = Cell.sink (9)
        let out = List<_>()
        (
            use _l = Transaction.Run (fun () -> c |> Operational.value |> Stream.listenOnce out.Add)
            c.Send 2
            c.Send 7
        )
        CollectionAssert.AreEqual([9], out)

    [<Test>]
    member __.``Test ListenOnceAsync``() =
        let test () = async {
            use c = Cell.sink (9)
            let! result = Transaction.Run (fun () -> c |> Operational.value |> Stream.listenOnceAsync)
            c.Send 2
            c.Send 7
            Assert.AreEqual(9, result)
        }
        test () |> Async.RunSynchronously

    [<Test>]
    member __.``Test Updates``() =
        use c = Cell.sink (9)
        let out = List<_>()
        (
            use _l = c |> Operational.updates |> Stream.listen out.Add
            c.Send 2
            c.Send 7
        )
        CollectionAssert.AreEqual([2;7], out)

    [<Test>]
    member __.``Test Value``() =
        use c = Cell.sink (9)
        let out = List<_>()
        (
            use _l = Transaction.Run (fun () -> c |> Operational.value |> Stream.listen out.Add)
            c.Send 2
            c.Send 7
        )
        CollectionAssert.AreEqual([9;2;7], out)

    [<Test>]
    member __.``Test Value Then Map``() =
        use c = Cell.sink (9)
        let out = List<_>()
        (
            use _l = Transaction.Run (fun () -> c |> Operational.value |> Stream.map ((+) 100) |> Stream.listen out.Add)
            c.Send 2
            c.Send 7
        )
        CollectionAssert.AreEqual([109;102;107], out)

    [<Test>]
    member __.``Test Value Then Merge``() =
        use c1 = Cell.sink (9)
        use c2 = Cell.sink (2)
        let out = List<_>()
        (
            use _l = Transaction.Run (fun () -> c1 |> Operational.value |> Stream.merge (+) (c2 |> Operational.value) |> Stream.listen out.Add)
            c1.Send 1
            c2.Send 4
        )
        CollectionAssert.AreEqual([11;1;4], out)

    [<Test>]
    member __.``Test Value Then Filter``() =
        use c = Cell.sink (9)
        let out = List<_>()
        (
            use _l = Transaction.Run (fun () -> c |> Operational.value |> Stream.filter ((Helper.flip (%) 2) >> ((<>) 0)) |> Stream.listen out.Add)
            c.Send 2
            c.Send 7
        )
        CollectionAssert.AreEqual([9;7], out)

    [<Test>]
    member __.``Test Value Then Once``() =
        use c = Cell.sink (9)
        let out = List<_>()
        (
            use _l = Transaction.Run (fun () -> c |> Operational.value |> Stream.once |> Stream.listen out.Add)
            c.Send 2
            c.Send 7
        )
        CollectionAssert.AreEqual([9], out)

    [<Test>]
    member __.``Test Value Then Late Listen``() =
        use c = Cell.sink (9)
        let out = List<_>()
        let value = c |> Operational.value
        c.Send 8
        (
            use _l = value |> Stream.listen out.Add
            c.Send 2
            c.Send 7
        )
        CollectionAssert.AreEqual([2;7], out)

    [<Test>]
    member __.``Test Map``() =
        use c = Cell.sink (6)
        let out = List<_>()
        (
            use _l = c |> Cell.map string |> Cell.listen out.Add
            c.Send 8
        )
        CollectionAssert.AreEqual(["6";"8"], out)

    [<Test>]
    member __.``Test Map Late Listen``() =
        use c = Cell.sink (6)
        let out = List<_>()
        use cm = c |> Cell.map string
        c.Send 2
        (
            use _l = cm |> Cell.listen out.Add
            c.Send 8
        )
        CollectionAssert.AreEqual(["2";"8"], out)

    [<Test>]
    member __.``Test Calm``() =
        use c = Cell.sink (2)
        let out = List<_>()
        (
            use _l = c |> Cell.calm |> Cell.listen out.Add
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 4
            c.Send 2
            c.Send 4
            c.Send 4
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 4
            c.Send 2
            c.Send 4
            c.Send 4
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 4
            c.Send 2
            c.Send 4
            c.Send 4
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 4
            c.Send 2
            c.Send 4
            c.Send 4
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 2
            c.Send 4
            c.Send 2
            c.Send 4
            c.Send 4
            c.Send 2
            c.Send 2
        )
        CollectionAssert.AreEqual([2;4;2;4;2;4;2;4;2;4;2;4;2;4;2;4;2;4;2;4;2], out)

    [<Test>]
    member __.``Test Calm 2``() =
        use c = Cell.sink (2)
        let out = List<_>()
        (
            use _l = c |> Cell.calm |> Cell.listen out.Add
            c.Send 4
            c.Send 2
            c.Send 4
            c.Send 4
            c.Send 2
            c.Send 2
        )
        CollectionAssert.AreEqual([2;4;2;4;2], out)

    [<Test>]
    member __.``Test Apply``() =
        use cf = Cell.sink (fun x -> "1 " + string x)
        use ca = Cell.sink 5L
        let out = List<_>()
        (
            use _l = ca |> Cell.apply cf |> Cell.listen out.Add
            cf.Send (fun x -> "12 " + string x)
            ca.Send 6L
        )
        CollectionAssert.AreEqual(["1 5";"12 5";"12 6"], out)

    [<Test>]
    member __.``Test Lift``() =
        use c1 = Cell.sink 1
        use c2 = Cell.sink 5L
        let out = List<_>()
        (
            use _l = Cell.lift2 (fun x y -> string x + " " + string y) c1 c2 |> Cell.listen out.Add
            c1.Send 12
            c2.Send 6L
        )
        CollectionAssert.AreEqual(["1 5";"12 5";"12 6"], out)

    [<Test>]
    member __.``Test Lift Glitch``() =
        use c1 = Cell.sink 1
        use c3 = c1 |> Cell.map ((*) 3)
        use c5 = c1 |> Cell.map ((*) 5)
        use c = Cell.lift2 (fun x y -> string x + " " + string y) c3 c5
        let out = List<_>()
        (
            use _l = c |> Cell.listen out.Add
            c1.Send 2
        )
        CollectionAssert.AreEqual(["3 5";"6 10"], out)

    [<Test>]
    member __.``Test Lift From Simultaneous``() =
        let (c1, c2) = Transaction.Run (fun () ->
            let c1 = Cell.sink 3
            let c2 = Cell.sink 5
            c2.Send 7
            (c1, c2))
        let out = List<_>()
        (
            use _l = Cell.lift2 (+) c1 c2 |> Cell.listen out.Add
            ()
        )
        CollectionAssert.AreEqual([10], out)

    [<Test>]
    member __.``Test Hold Is Delayed``() =
        let s = Stream.sink ()
        let h = s |> Stream.hold 0
        let pair = s |> Stream.snapshot (fun a b -> string a + " " + string b) h
        let out = List<_>()
        (
            use _l = pair |> Stream.listen out.Add
            s.Send 2
            s.Send 3
        )
        CollectionAssert.AreEqual(["2 0";"3 2"], out)

    [<Test>]
    member __.``Test Switch For Cell``() =
        let ssc = Stream.sink<Sc> ()
        let ca = ssc |> Stream.map (fun s -> s.a) |> Stream.filterOption |> Stream.hold 'A'
        let cb = ssc |> Stream.map (fun s -> s.b) |> Stream.filterOption |> Stream.hold 'a'
        let csw = ssc |> Stream.map (fun s -> s.sw) |> Stream.filterOption |> Stream.hold ca
        let co = csw |> Cell.switchC
        let out = List<_>()
        (
            use _l = co |> Cell.listen out.Add
            ssc.Send { a = Option.Some 'B'; b = Option.Some 'b'; sw = Option.None }
            ssc.Send { a = Option.Some 'C'; b = Option.Some 'c'; sw = Option.Some cb }
            ssc.Send { a = Option.Some 'D'; b = Option.Some 'd'; sw = Option.None }
            ssc.Send { a = Option.Some 'E'; b = Option.Some 'e'; sw = Option.Some ca }
            ssc.Send { a = Option.Some 'F'; b = Option.Some 'f'; sw = Option.None }
            ssc.Send { a = Option.None; b = Option.None; sw = Option.Some cb }
            ssc.Send { a = Option.None; b = Option.None; sw = Option.Some ca }
            ssc.Send { a = Option.Some 'G'; b = Option.Some 'g'; sw = Option.Some cb }
            ssc.Send { a = Option.Some 'H'; b = Option.Some 'h'; sw = Option.Some ca }
            ssc.Send { a = Option.Some 'I'; b = Option.Some 'i'; sw = Option.Some ca }
        )
        CollectionAssert.AreEqual(['A';'B';'c';'d';'E';'F';'f';'F';'g';'H';'I'], out)

    [<Test>]
    member __.``Test Switch For Cell Simultaneous``() =
        let sc1 : Sc2 = { c = Cell.sink 0 }
        let csc = Cell.sink sc1
        let co = csc |> Cell.map (fun b -> b.c) |> Cell.switchC
        let out = List<_>()
        (
            use _l = co |> Cell.listen out.Add
            let sc2 : Sc2 = { c = Cell.sink 3 }
            let sc3 : Sc2 = { c = Cell.sink 4 }
            let sc4 : Sc2 = { c = Cell.sink 7 }
            sc1.c.Send 1
            sc1.c.Send 2
            csc.Send sc2
            sc1.c.Send 3
            sc2.c.Send 4
            sc3.c.Send 5
            csc.Send sc3
            sc3.c.Send 6
            sc3.c.Send 7
            Transaction.Run (fun () ->
                sc3.c.Send 2
                csc.Send sc4
                sc4.c.Send 8)
            sc4.c.Send 9
        )
        CollectionAssert.AreEqual([0;1;2;3;4;5;6;7;8;9], out)

    [<Test>]
    member __.``Test Switch For Stream``() =
        let sss = Stream.sink<Ss> ()
        let sa = sss |> Stream.map (fun s -> s.a)
        let sb = sss |> Stream.map (fun s -> s.b)
        let csw = sss |> Stream.map (fun s -> s.sw) |> Stream.filterOption |> Stream.hold sa
        let so = csw |> Cell.switchS
        let out = List<_>()
        (
            use _l = so |> Stream.listen out.Add
            sss.Send { a = 'A'; b = 'a'; sw = Option.None }
            sss.Send { a = 'B'; b = 'b'; sw = Option.None }
            sss.Send { a = 'C'; b = 'c'; sw = Option.Some sb }
            sss.Send { a = 'D'; b = 'd'; sw = Option.None }
            sss.Send { a = 'E'; b = 'e'; sw = Option.Some sa }
            sss.Send { a = 'F'; b = 'f'; sw = Option.None }
            sss.Send { a = 'G'; b = 'g'; sw = Option.Some sb }
            sss.Send { a = 'H'; b = 'h'; sw = Option.Some sa }
            sss.Send { a = 'I'; b = 'i'; sw = Option.Some sa }
        )
        CollectionAssert.AreEqual(['A';'B';'C';'d';'e';'F';'G';'h';'I'], out)

    [<Test>]
    member __.``Test Switch For Stream Simultaneous``() =
        let ss1 : Ss2 = { s = Stream.sink () }
        let css = Cell.sink ss1
        let so = css |> Cell.map (fun b -> b.s) |> Cell.switchS
        let out = List<_>()
        (
            use _l = so |> Stream.listen out.Add
            let ss2 : Ss2 = { s = Stream.sink () }
            let ss3 : Ss2 = { s = Stream.sink () }
            let ss4 : Ss2 = { s = Stream.sink () }
            ss1.s.Send 0
            ss1.s.Send 1
            ss1.s.Send 2
            css.Send ss2
            ss1.s.Send 7
            ss2.s.Send 3
            ss2.s.Send 4
            ss3.s.Send 2
            css.Send ss3
            ss3.s.Send 5
            ss3.s.Send 6
            ss3.s.Send 7
            Transaction.Run (fun () ->
                ss3.s.Send 8
                css.Send ss4
                ss4.s.Send 2)
            ss4.s.Send 9
        )
        CollectionAssert.AreEqual([0;1;2;3;4;5;6;7;8;9], out)

    [<Test>]
    member __.``Test Switch Early For Stream``() =
        let ss1 : Ss2 = { s = Stream.sink () }
        let css = Cell.sink ss1
        let so = css |> Cell.map (fun b -> b.s) |> Cell.switchEarlyS
        let out = List<_>()
        (
            use _l = so |> Stream.listen out.Add
            let ss2 : Ss2 = { s = Stream.sink () }
            let ss3 : Ss2 = { s = Stream.sink () }
            let ss4 : Ss2 = { s = Stream.sink () }
            ss1.s.Send 0
            ss1.s.Send 1
            ss1.s.Send 2
            css.Send ss2
            ss1.s.Send 7
            ss2.s.Send 3
            ss2.s.Send 4
            ss3.s.Send 2
            css.Send ss3
            ss3.s.Send 5
            ss3.s.Send 6
            ss3.s.Send 7
            ss4.s.Send 8
            css.Send ss4
            ss4.s.Send 8
            ss3.s.Send 2
            ss4.s.Send 9
        )
        CollectionAssert.AreEqual([0;1;2;3;4;5;6;7;8;9], out)

    [<Test>]
    member __.``Test Switch Early For Stream Simultaneous``() =
        let ss1 : Ss2 = { s = Stream.sink () }
        let css = Cell.sink ss1
        let so = css |> Cell.map (fun b -> b.s) |> Cell.switchEarlyS
        let out = List<_>()
        (
            use _l = so |> Stream.listen out.Add
            let ss2 : Ss2 = { s = Stream.sink () }
            let ss3 : Ss2 = { s = Stream.sink () }
            let ss4 : Ss2 = { s = Stream.sink () }
            ss1.s.Send 0
            ss1.s.Send 1
            ss1.s.Send 2
            css.Send ss2
            ss1.s.Send 7
            ss2.s.Send 3
            ss2.s.Send 4
            ss3.s.Send 2
            css.Send ss3
            ss3.s.Send 5
            ss3.s.Send 6
            ss3.s.Send 7
            Transaction.Run (fun () ->
                ss4.s.Send 8
                css.Send ss4
                ss3.s.Send 2)
            ss4.s.Send 9
        )
        CollectionAssert.AreEqual([0;1;2;3;4;5;6;7;8;9], out)

    [<Test>]
    member __.``Test Lift List``() =
        let cellSinks = List.init 50 (fun _ -> Cell.sink 1)
        let sum = cellSinks |> Cell.liftAll List.sum
        let out = List<_>()
        (
            use _l = sum |> Cell.listen out.Add
            cellSinks.[4].Send 5
            cellSinks.[5].Send 5
            Transaction.Run (fun () ->
                cellSinks.[9].Send 5
                cellSinks.[17].Send 5
                cellSinks.[41].Send 5
                cellSinks.[48].Send 5)
        )
        CollectionAssert.AreEqual([50;54;58;74], out)

    [<Test>]
    member __.``Test Lift List Large``() =
        let cellSinks = List.init 500 (fun _ -> Cell.sink 1)
        let sum = cellSinks |> Cell.liftAll List.sum
        let out = List<_>()
        (
            use _l = sum |> Cell.listen out.Add
            cellSinks.[4].Send 5
            cellSinks.[5].Send 5
            Transaction.Run (fun () ->
                cellSinks.[9].Send 5
                cellSinks.[17].Send 5
                cellSinks.[41].Send 5
                cellSinks.[48].Send 5)
        )
        CollectionAssert.AreEqual([500;504;508;524], out)

    [<Test>]
    member __.``Test Lift List Large Many Updates``() =
        let cellSinks = List.init 500 (fun _ -> Cell.sink 1)
        let sum = cellSinks |> Cell.liftAll List.sum
        let out = List<_>()
        (
            use _l = sum |> Cell.listen out.Add
            for i = 0 to 99 do
                cellSinks.[i * 5].Send 5
                cellSinks.[i * 5 + 1].Send 5
                Transaction.Run (fun () ->
                    cellSinks.[i * 5 + 2].Send 5
                    cellSinks.[i * 5 + 3].Send 5
                    cellSinks.[i * 5 + 4].Send 5)
        )
        let expected = List.Cons (500, List.concat (List.init 100 (fun i -> [500 + 20 * i + 4; 500 + 20 * i + 8; 500 + 20 * i + 20])))
        CollectionAssert.AreEqual(expected, out)

    [<Test>]
    member __.``Test Lift Changes While Listening``() =
        let cellSinks = List.init 50 (fun _ -> Cell.sink 1)
        let sum = cellSinks |> Cell.liftAll List.sum
        let out = List<_>()
        let l = Transaction.Run (fun () ->
            cellSinks.[4].Send 5
            let l = sum |> Cell.listen out.Add
            cellSinks.[5].Send 5
            l)
        cellSinks.[9].Send 5
        Transaction.Run (fun () ->
            cellSinks.[17].Send 5
            cellSinks.[41].Send 5
            cellSinks.[48].Send 5)
        l.Unlisten ()
        CollectionAssert.AreEqual([58;62;74], out)
