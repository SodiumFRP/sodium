module SodiumFRP.FSharp.Tests.Cell

open System
open System.Collections.Generic
open System.Threading.Tasks
open NUnit.Framework
open SodiumFRP.FSharp
open SodiumFRP.FSharp.Sodium

type private Test (initialValue : int) = member val Value = sinkC initialValue
type private Inner = { s : StreamSink<int>; c : Cell<int> }

[<TestFixture>]
type Tests() =

    [<Test>]
    member __.``Test Loop``() =
        let (_, (c, s)) = loopC (fun cl ->
            let c = cl |> mapC ((*) 5)
            let s = sinkCS<int> ()
            (s |> holdS 3, (c, s)))
        let output1 = List<_>()
        let output2 = List<_>()
        let l = c |> listenC output1.Add
        let l2 = c |> updatesC |> listenS output2.Add
        s |> sendS 5
        s |> sendS 7
        l2 |> unlistenL
        l |> unlistenL
        CollectionAssert.AreEqual([15;25;35], output1)
        CollectionAssert.AreEqual([25;35], output2)
