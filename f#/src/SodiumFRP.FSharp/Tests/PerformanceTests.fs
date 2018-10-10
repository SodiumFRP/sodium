module SodiumFRP.FSharp.Tests.Performance

open System.Collections.Generic
open NUnit.Framework
open SodiumFRP.FSharp
open SodiumFRP.FSharp.Sodium

[<AutoOpen>]
module private Types =
    
    type TestObject (s : Stream<bool>, s1 : Stream<int>, s2 : Stream<int>) as this =
        let mutable currentValue = lazy 0
        let (cell, l) = runT (fun () ->
            let cell = ((s |> mapS (fun v -> if v then 1 else 0), s1) |> orElseS, s2) |> orElseS |> holdS 0
            let createCell () =
                (
                    s1 |> snapshotC cell (+) |> filterS (flip (>) 5),
                    s |> snapshotAndTakeC ((s1 |> holdS 0, s2 |> holdS 1) |> lift2C (+)) |> mapS ((+) 1)
                ) |> orElseS |> holdS 3
            let cell2 = createCell ()
            let cell3 = createCell ()
            let cell4 = createCell ()
            let cell5 = createCell ()
            let cell6 = createCell ()
            let cell7 = createCell ()
            let cell8 = createCell ()
            let cell9 = createCell ()
            currentValue <- cell |> sampleLazyC
            let l = cell |> updatesC |> listenS (fun v -> this.CurrentValue <- v)
            (cell, l))
        member private __.L = l
        member __.Cell = cell
        member __.CurrentValue with get () = currentValue.Value and set value = currentValue <- lazy value

[<TestFixture>]
type Tests() =

    [<Test>]
    member __.``Test Merge``() =
        let s = sinkS<unit> ()
        let struct (_, obj) = loopS (fun loop ->
            let s1 = sinkCS ()
            let s2 = sinkCS ()
            let l = Array.init 250 (fun _ -> TestObject (loop, s1, s2))
            struct (s |> snapshotAndTakeC (l |> Seq.map (fun o -> o.Cell) |> liftAllC id) |> mapS (Seq.forall (fun v -> v = 0)), l))
        let values = obj |> Array.map (fun o -> o.CurrentValue)
        CollectionAssert.AreEqual (Seq.init 250 (fun _ -> 0), values)