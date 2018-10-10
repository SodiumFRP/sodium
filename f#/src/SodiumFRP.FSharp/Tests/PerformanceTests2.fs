module SodiumFRP.FSharp.Tests.Performance2

open System.Collections.Generic
open NUnit.Framework
open Sodium

[<AutoOpen>]
module private Types =
    
    type TestObject (s : Stream<bool>, s1 : Stream<int>, s2 : Stream<int>) as this =
        let mutable currentValue = lazy 0
        let (cell, l) = Transaction.Run (fun () ->
            let cell = s.Map(fun v -> if v then 1 else 0).OrElse(s1).OrElse(s2).Hold(0)
            let createCell () =
                s1.Snapshot(cell, (+)).Filter(fun v -> v > 5).OrElse(s.Snapshot((s1.Hold(0).Lift(s2.Hold(1), (fun x y -> x + y))).Map(fun v -> v + 1))).Hold(3)
            let cell2 = createCell ()
            let cell3 = createCell ()
            let cell4 = createCell ()
            let cell5 = createCell ()
            let cell6 = createCell ()
            let cell7 = createCell ()
            let cell8 = createCell ()
            let cell9 = createCell ()
            currentValue <- cell.SampleLazy()
            let l = cell.Updates.Listen(fun v -> this.CurrentValue <- v)
            (cell, l))
        member private __.L = l
        member __.Cell = cell
        member __.CurrentValue with get () = currentValue.Value and set value = currentValue <- lazy value

[<TestFixture>]
type Tests() =

    [<Test>]
    member __.``Test Merge``() =
        let s = StreamSink<unit> ()
        let struct (_, obj) = Stream.Loop<bool>().WithCaptures (fun loop ->
            let s1 = CellStreamSink<_> ()
            let s2 = CellStreamSink<_> ()
            let l = Array.init 5000 (fun _ -> TestObject (loop, s1, s2))
            struct (s.Snapshot((l |> Seq.map (fun o -> o.Cell)).Lift()).Map(fun v -> v |> Seq.forall (fun v -> v = 0)), l))
        let values = obj |> Array.map (fun o -> o.CurrentValue)
        CollectionAssert.AreEqual (Seq.init 5000 (fun _ -> 0), values)