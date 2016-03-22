module Sodium.Tests.``Denotational Semantics``

open NUnit.Framework
open Sodium
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
                | _ ->
                    printfn "Test failed for ordering { %s }." (list |> List.map fst |> String.concat ", ")
                    reraise ()

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

    [<Test>]
    member __.``Filter: Test Case``() =
        let (s, sf) = mkStream ([(0,5);(1,6);(2,7)] |> Map.ofList)
        let out = runSimulation (Helper.flip Stream.listen (s |> Stream.filter ((Helper.flip (%) 2) >> ((<>) 0)))) [sf]
        CollectionAssert.AreEqual([5;7], out)

    [<Test>]
    member __.``SwitchS: Test Case``() =
        runPermutations
            (fun () ->
                let (s1, s1f) = mkStream ([(0,'a');(1,'b');(2,'c');(3,'d')] |> Map.ofList)
                let (s2, s2f) = mkStream ([(0,'W');(1,'X');(2,'Y');(3,'Z')] |> Map.ofList)
                let (switcher, switcherF) = mkStream ([1,s2] |> Map.ofList)
                let c = switcher |> Stream.hold s1
                let firings = [("s1",s1f);("s2",s2f);("switcher",switcherF)]
                (firings, Helper.flip Stream.listen (c |> Cell.switchS)))
            (fun out -> CollectionAssert.AreEqual(['a';'b';'Y';'Z'], out))

    [<Test>]
    member __.``Updates: Test Case``() =
        let (s, sf) = mkStream ([(1,'b');(3,'c')] |> Map.ofList)
        let c = s |> Stream.hold 'a'
        let out = runSimulation (Helper.flip Stream.listen (c |> Operational.updates)) [sf]
        CollectionAssert.AreEqual(['b';'c'], out)

    [<Test>]
    member __.``Value: Test Case 1``() =
        let (s, sf) = mkStream ([(1,'b');(3,'c')] |> Map.ofList)
        let c = s |> Stream.hold 'a'
        let out = runSimulation (fun h -> Transaction.Run (fun () -> c |> Operational.value |> Stream.listen h)) [sf]
        CollectionAssert.AreEqual(['a';'b';'c'], out)

    [<Test>]
    member __.``Value: Test Case 2``() =
        let (s, sf) = mkStream ([(0,'b');(1,'c');(3,'d')] |> Map.ofList)
        let c = s |> Stream.hold 'a'
        let out = runSimulation (fun h -> Transaction.Run (fun () -> c |> Operational.value |> Stream.listen h)) [sf]
        CollectionAssert.AreEqual(['b';'c';'d'], out)

    [<Test>]
    member __.``ListenC: Test Case 1``() =
        let (s, sf) = mkStream ([(1,'b');(3,'c')] |> Map.ofList)
        let c = s |> Stream.hold 'a'
        let out = runSimulation (Helper.flip Cell.listen c) [sf]
        CollectionAssert.AreEqual(['a';'b';'c'], out)

    [<Test>]
    member __.``ListenC: Test Case 2``() =
        let (s, sf) = mkStream ([(0,'b');(1,'c');(3,'d')] |> Map.ofList)
        let c = s |> Stream.hold 'a'
        let out = runSimulation (Helper.flip Cell.listen c) [sf]
        CollectionAssert.AreEqual(['b';'c';'d'], out)

    [<Test>]
    member __.``Split: Test Case``() =
        let (s, sf) = mkStreamWithCoalesce [(0,['a';'b']);(1,['c']);(1,['d';'e'])] List.append
        let out = runSimulationWithSimultaneousFirings (Helper.flip Stream.listen (s |> Operational.split)) [sf]
        CollectionAssert.AreEqual(['a';'b';'c';'d';'e'], out)

    [<Test>]
    member __.``Constant: Test Case``() =
        let c = Cell.constant 'a'
        let out = runSimulationWithNoFirings (Helper.flip Cell.listen c)
        CollectionAssert.AreEqual(['a'], out)

    [<Test>]
    member __.``ConstantLazy: Test Case``() =
        let c = Cell.constantLazy (lazy 'a')
        let out = runSimulationWithNoFirings (Helper.flip Cell.listen c)
        CollectionAssert.AreEqual(['a'], out)

    [<Test>]
    member __.``Hold: Test Case``() =
        let (s, sf) = mkStream ([(1,'b');(3,'c')] |> Map.ofList)
        let c = s |> Stream.hold 'a'
        let out = runSimulation (Helper.flip Cell.listen c) [sf]
        CollectionAssert.AreEqual(['a';'b';'c'], out)

    [<Test>]
    member __.``MapC: Test Case``() =
        let (s, sf) = mkStream ([(2,3);(3,5)] |> Map.ofList)
        let c = s |> Stream.hold 0
        let out = runSimulation (Helper.flip Cell.listen (c |> Cell.map ((+) 1))) [sf]
        CollectionAssert.AreEqual([1;4;6], out)

    [<Test>]
    member __.``Apply: Test Case``() =
        let (s1, s1f) = mkStream ([(1,200);(2,300);(4,400)] |> Map.ofList)
        let ca = s1 |> Stream.hold 100
        let (s2, s2f) = mkStream ([(1,(+) 5);(3,(+) 6)] |> Map.ofList)
        let cf = s2 |> Stream.hold ((+) 0)
        let out = runSimulation (Helper.flip Cell.listen (ca |> Cell.apply cf)) [s1f;s2f]
        CollectionAssert.AreEqual([100;205;305;306;406], out)

    [<Test>]
    member __.``SwitchC: Test Case 1``() =
        runPermutations
            (fun () ->
                let (s1, s1f) = mkStream ([(0,'b');(1,'c');(2,'d');(3,'e')] |> Map.ofList)
                let c1 = s1 |> Stream.hold 'a'
                let (s2, s2f) = mkStream ([(0,'W');(1,'X');(2,'Y');(3,'Z')] |> Map.ofList)
                let c2 = s2 |> Stream.hold 'V'
                let (switcher, switcherF) = mkStream ([1,c2] |> Map.ofList)
                let c = switcher |> Stream.hold c1
                let firings = [("s1",s1f);("s2",s2f);("switcher",switcherF)]
                (firings, Helper.flip Cell.listen (c |> Cell.switchC)))
            (fun out -> CollectionAssert.AreEqual(['b';'X';'Y';'Z'], out))

    [<Test>]
    member __.``SwitchC: Test Case 2``() =
        runPermutations
            (fun () ->
                let (s1, s1f) = mkStream ([(0,'b');(1,'c');(2,'d');(3,'e')] |> Map.ofList)
                let c1 = s1 |> Stream.hold 'a'
                let (s2, s2f) = mkStream ([(1,'X');(2,'Y');(3,'Z')] |> Map.ofList)
                let c2 = s2 |> Stream.hold 'W'
                let (switcher, switcherF) = mkStream ([1,c2] |> Map.ofList)
                let c = switcher |> Stream.hold c1
                let firings = [("s1",s1f);("s2",s2f);("switcher",switcherF)]
                (firings, Helper.flip Cell.listen (c |> Cell.switchC)))
            (fun out -> CollectionAssert.AreEqual(['b';'X';'Y';'Z'], out))

    [<Test>]
    member __.``SwitchC: Test Case 3``() =
        runPermutations
            (fun () ->
                let (s1, s1f) = mkStream ([(0,'b');(1,'c');(2,'d');(3,'e')] |> Map.ofList)
                let c1 = s1 |> Stream.hold 'a'
                let (s2, s2f) = mkStream ([(2,'Y');(3,'Z')] |> Map.ofList)
                let c2 = s2 |> Stream.hold 'X'
                let (switcher, switcherF) = mkStream ([1,c2] |> Map.ofList)
                let c = switcher |> Stream.hold c1
                let firings = [("s1",s1f);("s2",s2f);("switcher",switcherF)]
                (firings, Helper.flip Cell.listen (c |> Cell.switchC)))
            (fun out -> CollectionAssert.AreEqual(['b';'X';'Y';'Z'], out))

    [<Test>]
    member __.``SwitchC: Test Case 4``() =
        runPermutations
            (fun () ->
                let (s1, s1f) = mkStream ([(0,'b');(1,'c');(2,'d');(3,'e')] |> Map.ofList)
                let c1 = s1 |> Stream.hold 'a'
                let (s2, s2f) = mkStream ([(0,'W');(1,'X');(2,'Y');(3,'Z')] |> Map.ofList)
                let c2 = s2 |> Stream.hold 'V'
                let (s3, s3f) = mkStream ([(0,'2');(1,'3');(2,'4');(3,'5')] |> Map.ofList)
                let c3 = s3 |> Stream.hold '1'
                let (switcher, switcherF) = mkStream ([(1,c2);(3,c3)] |> Map.ofList)
                let c = switcher |> Stream.hold c1
                let firings = [("s1",s1f);("s2",s2f);("s3",s3f);("switcher",switcherF)]
                (firings, Helper.flip Cell.listen (c |> Cell.switchC)))
            (fun out -> CollectionAssert.AreEqual(['b';'X';'Y';'5'], out))

    [<Test>]
    member __.``Sample: Test Case``() =
        let s = Stream.sink ()
        let c = s |> Stream.hold 'a'
        let sample1 = c |> Cell.sample
        s.Send 'b'
        let sample2 = c |> Cell.sample
        Assert.AreEqual('a', sample1)
        Assert.AreEqual('b', sample2)

    [<Test>]
    member __.``SampleLazy: Test Case``() =
        let s = Stream.sink ()
        let c = s |> Stream.hold 'a'
        let sample1 = c |> Cell.sampleLazy
        s.Send 'b'
        let sample2 = c |> Cell.sampleLazy
        Assert.AreEqual('a', sample1.Value)
        Assert.AreEqual('b', sample2.Value)
