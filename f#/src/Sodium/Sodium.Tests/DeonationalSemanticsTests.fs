module Sodium.Tests.``Deonational Semantics``

open NUnit.Framework
open Sodium
open System
open System.Collections.Generic

[<TestFixture>]
type Tests() =
    
    let runSimulationWithSimultaneousFirings listen firings =
        let maxKey =
            match firings with
            | [] -> -1
            | _ -> firings |> Seq.collect (Map.toSeq >> Seq.map fst) |> Seq.max
        let out = List<_>()
        let run t = for a in firings |> Seq.collect (fun f -> match f |> Map.tryFind t with | None -> [] | Some t -> t) do a ()
        match maxKey with
        | -1 ->
            use _l = listen out.Add
            ()
        | _ ->
            use _l = Transaction.Run (fun () ->
                let l = listen out.Add
                run 0
                l)
            for i = 1 to maxKey do Transaction.Run (fun () -> run i)
        out

    let runSimulationWithNoFirings listen =
        runSimulationWithSimultaneousFirings listen List.empty

    let runSimulation listen firings =
        runSimulationWithSimultaneousFirings listen (firings |> List.map (Map.map (fun _ a -> [a])))

    let mkStream firings =
        let s = Stream.sink ()
        let f = firings |> Map.map (fun _ v -> (fun () -> s.Send v))
        match f |> Map.tryFindKey (fun k _ -> k < 0) with
        | None -> ()
        | Some _ -> invalidOp "All firings must occur at T >= 0."
        (s :> 'a Stream, f)

    let mkStreamWithCoalesce firings coalesce =
        let s = Stream.sinkWithCoalesce coalesce
        let f = firings |> List.groupBy fst |> Map.ofList |> Map.map (fun _ v -> v |> (List.map (fun v -> (fun () -> s.Send (snd v)))))
        match f |> Map.tryFindKey (fun k _ -> k < 0) with
        | None -> ()
        | Some _ -> invalidOp "All firings must occur at T >= 0."
        (s :> 'a Stream, f)

    let rec getPermutationsInternal list length =
        if length = 1
        then list |> List.map (fun o -> [o])
        else getPermutationsInternal list (length - 1) |> List.collect (fun t -> list |> List.filter (fun e -> t |> List.contains e |> not) |> List.map (fun t2 -> List.append t [t2]))

    let getPermutations list = getPermutationsInternal list list.Length

    let runPermutations
        //(createListAndListener : ((string * Map<int, (unit -> unit)>) list -> (('T -> unit) -> IListener) -> (string * (int * (unit -> unit)) Map) list * (('T -> unit) -> IListener)) -> (string * Map<int, (unit -> unit)>) list * (('T -> unit) -> IListener))
        (createListAndListener : unit -> (string * Map<int, (unit -> unit)>) list * (('T -> unit) -> IListener))
        ``assert`` =
        let indexes = List.init (createListAndListener () |> fst).Length id
        for (list, listener) in (getPermutations indexes |> List.map (fun ii ->
            let (list, listener) = createListAndListener ()
            (ii |> List.map (fun i -> list.[i]), listener))) do
            try
                let out = runSimulation listener (list |> List.map snd)
                ``assert`` out
            with
                | e -> printfn "Test failed for ordering { %s }." (list |> List.map fst |> String.concat ", ")

    [<Test>]
    member __.``Never: Test Case``() =
        let out = runSimulationWithNoFirings (Helper.flip Stream.listen (Stream.never<int> ()))
        CollectionAssert.AreEqual([], out)

    [<Test>]
    member __.``MapS: Test Case``() =
        let (s, sf) = mkStream ([(0,5);(1,10);(2,12)] |> Map.ofList)
        let out = runSimulation (Helper.flip Stream.listen (s |> Stream.map ((+) 1))) [sf]
        CollectionAssert.AreEqual([6;11;13], out)

    [<Test>]
    member __.``Snapshot: Test Case``() =
        let (s1, s1f) = mkStream ([(0,'a');(3,'b');(5,'c')] |> Map.ofList)
        let (s2, s2f) = mkStream ([(1,4);(5,7)] |> Map.ofList)
        let c = s2 |> Stream.hold 3
        let out = runSimulation (Helper.flip Stream.listen (s1 |> Stream.snapshotAndTakeCell c)) [s1f;s2f]
        CollectionAssert.AreEqual([3;4;4], out)

    [<Test>]
    member __.``Merge: Test Case``() =
        let (s1, s1f) = mkStream ([(0,0);(2,2)] |> Map.ofList)
        let (s2, s2f) = mkStream ([(1,10);(2,20);(3,30)] |> Map.ofList)
        let out = runSimulation (Helper.flip Stream.listen (s1 |> Stream.merge (+) s2)) [s1f;s2f]
        CollectionAssert.AreEqual([0;10;22;30], out)
