module Sodium.Frp.Tests.Performance

open System.Collections.Generic
open NUnit.Framework
open Sodium.Frp

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
    
    type TestObject2 (id : int, initialIsSelected : bool, selectAllStream : Stream<bool>) =
        let isSelectedStreamSink = sinkS ()
        let isSelected = (selectAllStream, isSelectedStreamSink) |> orElseS |> holdS initialIsSelected
        member __.Id = id
        member __.IsSelectedStreamSink = isSelectedStreamSink
        member __.IsSelected = isSelected

[<TestFixture>]
type ``Performance Tests``() =

    [<Test>]
    member __.``Test Merge``() =
        let s = sinkS<unit> ()
        let struct (_, obj) = loopS (fun loop ->
            let s1 = sinkCS ()
            let s2 = sinkCS ()
            let l = Array.init 5000 (fun _ -> TestObject (loop, s1, s2))
            struct (s |> snapshotAndTakeC (l |> Seq.map (fun o -> o.Cell) |> liftAllC id) |> mapS (Seq.forall (fun v -> v = 0)), l))
        let values = obj |> Array.map (fun o -> o.CurrentValue)
        CollectionAssert.AreEqual (Seq.init 5000 (fun _ -> 0), values)
    
    [<Test>]
    member __.``Test Run Construct``() =
        let objects = runT (fun () ->
            let o2 = List.init 10000 (fun n -> TestObject2 (n, n < 1500, neverS ()))
            sinkC o2)
        runT (fun () -> objects |> sendC (List.init 20000 (fun n -> TestObject2 (n, n < 500, neverS ()))))
    
    [<Test>]
    member __.``Test Run Construct 2``() =
        let struct (_, (objectsAndIsSelected, selectAllStream, objects)) = loopC (fun allSelected ->
            let toggleAllSelectedStream = sinkS ()
            let selectAllStream = toggleAllSelectedStream |> snapshotAndTakeC allSelected |> mapS (fun a -> match a with | Some a -> not a | None -> true)
            let o2 = List.init 10000 (fun n -> TestObject2 (n, n < 1500, selectAllStream))
            let objects = sinkC o2
            let objectsAndIsSelected = objects |> mapC (Seq.map (fun o -> o.IsSelected |> mapC (fun s -> (o, s))) >> liftAllC id) |> switchC
            let defaultValue = Some (o2.Length < 1)
            let allSelected = objectsAndIsSelected |> mapC (fun oo -> if oo.Count > 0 then (if oo |> Seq.forall (fun (_, isSelected) -> isSelected) then Some true else (if oo |> Seq.forall (fun (_, isSelected) -> not isSelected) then Some false else None)) else defaultValue)
            struct (allSelected, (objectsAndIsSelected, selectAllStream, objects)))
        let out = List<_>()
        (
            use _l = runT (fun () -> objectsAndIsSelected |> mapC (Seq.where (fun (_, isSelected) -> isSelected) >> Seq.length) |> listenC out.Add)
            runT (fun () -> objects |> sendC (List.init 20000 (fun n -> TestObject2 (n, n < 500, selectAllStream))))
        )
        CollectionAssert.AreEqual ([1500;500], out)