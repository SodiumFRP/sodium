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
        let run t = for a in firings |> Seq.collect (Map.find t) do a ()
        match maxKey with
        | -1 ->
            use _l = Transaction.Run (fun () ->
                let l = listen out.Add
                run 0
                l)
            for i = 1 to maxKey do Transaction.Run (fun () -> run i)
        | _ ->
            use _l = listen out.Add
            ()
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

    [<Test>]
    member __.``Never: Test Case``() =
        use s = Stream.sink ()
        let out = List<_>()
        (
            use _l = s |> Stream.listen out.Add
            s.Send 5
        )
        CollectionAssert.AreEqual([5], out)
        s.Send 6
        CollectionAssert.AreEqual([5], out)
