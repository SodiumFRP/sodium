module SodiumFRP.FSharp.Tests.Issue

open System.Collections.Generic
open NUnit.Framework
open SodiumFRP.FSharp
open SodiumFRP.FSharp
open SodiumFRP.FSharp.Sodium

[<TestFixture>]
type Tests() =

    let flip f x y = f y x

    [<Test>]
    member __.``Issue 151: Pool Double Subtraction: Broken``() =
        let actual =
            try
                let threshold = sinkC 10
                let addPoolSink = sinkS ()
                let (submitPooledAmount, pool) = loopS (fun submitPooledAmount ->
                    let poolAddByInput = addPoolSink |> mapS (flip (+))
                    let poolremoveByUsage = submitPooledAmount |> mapS (fun x -> (flip (-) x))
                    let pool = (poolAddByInput, poolremoveByUsage) |> mergeS (>>) |> accumS 0 (<|)
                    let inputByAdded = poolAddByInput |> snapshot2C pool threshold (fun f x t ->
                        let r = f x
                        if r >= t then Some r else None) |> filterOptionS
                    let inputBySatisfaction = pool |> updatesC |> snapshot2C pool threshold (fun neu alt t ->
                        if neu >= t && alt < t then Some neu else None) |> filterOptionS
                    ((inputByAdded, inputBySatisfaction) |> mergeS max, pool))
                None
            with
                | e -> Some e
        actual |> assertExceptionExists (fun e -> Assert.AreEqual ("A dependency cycle was detected.", e.Message))

    [<Test>]
    member __.``Issue 151: Pool Double Subtraction: Fixed``() =
        let threshold = sinkC 10
        let addPoolSink = sinkS ()
        let (input, pool) = loopS (fun submitPooledAmount ->
            let poolAddByInput = addPoolSink |> mapS (flip (+))
            let poolremoveByUsage = submitPooledAmount |> mapS (fun x -> (flip (-) x)) |> Operational.defer
            let pool = (poolAddByInput, poolremoveByUsage) |> mergeS (>>) |> accumS 0 (<|)
            let inputByAdded = poolAddByInput |> snapshot2C pool threshold (fun f x t ->
                let r = f x
                if r >= t then Some r else None) |> filterOptionS
            let inputBySatisfaction = pool |> updatesC |> snapshot2C pool threshold (fun neu alt t ->
                if neu >= t && alt < t then Some neu else None) |> filterOptionS
            ((inputByAdded, inputBySatisfaction) |> mergeS max, pool))
        let submissions = List<_>()
        let l = input |> listenS submissions.Add
        addPoolSink |> sendS 10
        l |> unlistenL
        Assert.AreEqual (1, submissions.Count)
        Assert.AreEqual (10, submissions.[0])
        Assert.AreEqual (0, pool |> sampleC)