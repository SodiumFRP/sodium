module Sodium.Frp.Tests.Issue

open System.Collections.Generic
open NUnit.Framework
open Sodium.Frp

module Issue151 =

    [<TestFixture>]
    type ``Issue 151 Tests``() =
    
        [<Test>]
        member __.``Pool Double Subtraction: Broken``() =
            let actual =
                try
                    let threshold = sinkC 10
                    let addPoolSink = sinkS ()
                    let struct (submitPooledAmount, pool) = loopS (fun submitPooledAmount ->
                        let poolAddByInput = addPoolSink |> mapS (flip (+))
                        let poolremoveByUsage = submitPooledAmount |> mapS (fun x -> (flip (-) x))
                        let pool = (poolAddByInput, poolremoveByUsage) |> mergeS (>>) |> accumS 0 (<|)
                        let inputByAdded = poolAddByInput |> snapshot2C pool threshold (fun f x t ->
                            let r = f x
                            if r >= t then Some r else None) |> filterOptionS
                        let inputBySatisfaction = pool |> updatesC |> snapshot2C pool threshold (fun neu alt t ->
                            if neu >= t && alt < t then Some neu else None) |> filterOptionS
                        struct ((inputByAdded, inputBySatisfaction) |> mergeS max, pool))
                    None
                with
                    | e -> Some e
            actual |> assertExceptionExists (fun e -> Assert.AreEqual ("A dependency cycle was detected.", e.Message))
    
        [<Test>]
        member __.``Pool Double Subtraction: Fixed``() =
            let threshold = sinkC 10
            let addPoolSink = sinkS ()
            let struct (input, pool) = loopS (fun submitPooledAmount ->
                let poolAddByInput = addPoolSink |> mapS (flip (+))
                let poolremoveByUsage = submitPooledAmount |> mapS (fun x -> (flip (-) x)) |> Operational.defer
                let pool = (poolAddByInput, poolremoveByUsage) |> mergeS (>>) |> accumS 0 (<|)
                let inputByAdded = poolAddByInput |> snapshot2C pool threshold (fun f x t ->
                    let r = f x
                    if r >= t then Some r else None) |> filterOptionS
                let inputBySatisfaction = pool |> updatesC |> snapshot2C pool threshold (fun neu alt t ->
                    if neu >= t && alt < t then Some neu else None) |> filterOptionS
                struct ((inputByAdded, inputBySatisfaction) |> mergeS max, pool))
            let submissions = List<_>()
            let l = input |> listenS submissions.Add
            addPoolSink |> sendS 10
            l |> unlistenL
            Assert.AreEqual (1, submissions.Count)
            Assert.AreEqual (10, submissions.[0])
            Assert.AreEqual (0, pool |> sampleC)

module Issue138 =
    open System

    [<AutoOpen>]
    module private Types =
    
        type TestObject = private { Input1' : StreamSink<int>; Input2' : StreamSink<int>; Output' : Cell<int> }
            with
                static member create () =
                    let input1 = sinkCS ()
                    let input1Cell = input1 |> holdS 3
                    let input2 = sinkCS ()
                    let input2Cell = input2 |> holdS 2
                    let output = (input1Cell, input2Cell) |> lift2C (+)
                    { Input1' = input1; Input2' = input2; Output' = output }
                member this.Input1 = this.Input1'
                member this.Input2 = this.Input2'
                member this.Output = this.Output'
        
        [<RequireQualifiedAccess>]
        module TestObject =
            let input1 (o : TestObject) = o.Input1
            let input2 (o : TestObject) = o.Input2
            let output (o : TestObject) = o.Output
    
    (*
     * Desired behavior:
     *     A list of items of type TestObject are held in a cell.  TestObject contains a cell of type int named Output, which is calculated from other values.
     *     Any time a new TestObject is created, it will have the values for the cells from which Output is calculated.  The sum of all Output values in the list should always be 50 or greater.
     *)
    [<TestFixture>]
    type ``Issue 138 Tests``() =
    
        (*
         * Switch over the sum of the Output cells in the list.
         * This won't work because we would need to recurse to keep the list correct when the sum is very low (only one item can be added per transaction).
         * The current implementation throws an exception stating that a dependency cycle was detected, and I think this is the correct behavior.
         *)
        [<Test>]
        member __.``Test SwitchC Loop``() =
            let actual =
                try
                    let streamSink = sinkCS ()
                    let cell : Cell<TestObject list> =
                        loopWithNoCapturesC (fun cell ->
                            (
                                streamSink |> mapS (fun v _ -> v),
                                cell
                                    |> mapC (Seq.map TestObject.output >> liftAllC Seq.sum)
                                    |> switchC
                                    |> updatesC
                                    |> filterS (flip (<) 50)
                                    |> mapToS (List.append (List.singleton <| TestObject.create ()))
                            ) |> mergeS (>>)
                            |> snapshotC cell (<|)
                            |> holdS (List.init 10 (fun _ -> TestObject.create ())))
                    None
                with
                    | :? AggregateException as e ->
                        e.InnerExceptions |> Seq.tryFind (fun e -> e.Message = "A dependency cycle was detected.")
                    | e -> Some e
            actual |> assertExceptionExists (fun e -> Assert.AreEqual ("A dependency cycle was detected.", e.Message))
    
        (*
         * Switch over the sum of the Output cell value streams in the list.
         * This won't work both because we miss the first Values stream event when the list changes and also because we would need to recurse to keep the list correct when the sum is very low (only one item can be added per transaction).
         *)
        [<Test>]
        member __.``Test SwitchS Values Loop``() =
            let streamSink = sinkCS ()
            let cell : Cell<TestObject list> =
                loopWithNoCapturesC (fun cell ->
                    (
                        streamSink |> mapS (fun v _ -> v),
                        cell
                            |> mapC (Seq.map TestObject.output >> liftAllC Seq.sum >> valuesC)
                            |> switchS
                            |> filterS (flip (<) 50)
                            |> mapToS (List.append (List.singleton <| TestObject.create ()))
                    ) |> mergeS (>>)
                    |> snapshotC cell (<|)
                    |> holdS (List.init 10 (fun _ -> TestObject.create ())))
            let objectCounts = List<_>()
            objectCounts.Add -1
            let l = cell |> listenC (objectCounts.Add << List.length)
            objectCounts.Add -1
            (cell |> sampleC).[2].Input1 |> sendS 1
            objectCounts.Add -1
            (cell |> sampleC).[1].Input1 |> sendS -20
            objectCounts.Add -1
            streamSink |> sendS List.empty
            objectCounts.Add -1
            l |> unlistenL
            
            // Ideal result, likely not achievable.
            //CollectionAssert.AreEquivalent ([-1;10;-1;11;-1;15;-1;10;-1], objectCounts)

            // Glitchy result, also not returned by this method.
            //CollectionAssert.AreEquivalent ([-1;10;-1;11;-1;12;13;14;15;-1;0;1;2;3;4;5;6;7;8;9;10;-1], objectCounts)

            // Incorrect result we will see.
            CollectionAssert.AreEquivalent ([-1;10;-1;11;-1;12;-1;0;-1], objectCounts)
    
        (*
         * Switch over the sum of the Output cells in the list, deferring the firings from the Values stream.
         * This will work because it allows the Values to recurse by firing each step in a new transaction immediately following the transaction for the previous step.
         * The only drawback to this method is that each step of the recursion is in a new transaction, so it exhibits "glitchy" behavior where the intermediate invalid states are externally visible.
         *)
        [<Test>]
        member __.``Test SwitchC Deferred Loop``() =
            let streamSink = sinkCS ()
            let cell : Cell<TestObject list> =
                loopWithNoCapturesC (fun cell ->
                    (
                        streamSink,
                        cell
                            |> mapC (Seq.map TestObject.output >> liftAllC Seq.sum)
                            |> switchC
                            |> valuesC
                            |> Operational.defer
                            |> filterS (flip (<) 50)
                            |> mapToS (List.append (List.singleton <| TestObject.create ()))
                            |> snapshotC cell (<|)
                    ) |> orElseS
                    |> holdS (List.init 10 (fun _ -> TestObject.create ())))
            let objectCounts = List<_>()
            objectCounts.Add -1
            let l = cell |> listenC (objectCounts.Add << List.length)
            objectCounts.Add -1
            (cell |> sampleC).[2].Input1 |> sendS 1
            objectCounts.Add -1
            (cell |> sampleC).[1].Input1 |> sendS -20
            objectCounts.Add -1
            streamSink |> sendS List.empty
            objectCounts.Add -1
            l |> unlistenL
            
            // Ideal result, likely not achievable.
            //CollectionAssert.AreEquivalent ([-1;10;-1;11;-1;15;-1;10;-1], objectCounts)

            // Glitchy result, but correct otherwise.
            CollectionAssert.AreEquivalent ([-1;10;-1;11;-1;12;13;14;15;-1;0;1;2;3;4;5;6;7;8;9;10;-1], objectCounts)

        (*
         * Switch over the sum of the Output cells in the list, deferring the firings from the Values stream, and use a better API.
         * This is identical to the previous solution except that it uses a special version of SwitchC which defers the Values stream firings.
         * Using this API, we can better capture the intent of the SwitchC call and allow the type system to eventually check for valid usages of the looped cell.
         * Note that when the types are modified, the SwitchCWithDeferredValues() call will actually become SwitchC().DeferredValues() with SwitchC() on the cell loop returning a special type
         * containing the DeferredValues() and DeferredUpdates() methods.
         *)
        [<Test>]
        member __.``Test SwitchC Deferred Loop With Better API``() =
            let switchCWithDeferredValues cell = cell |> switchC |> valuesC |> Operational.defer
            let streamSink = sinkCS ()
            let cell : Cell<TestObject list> =
                loopWithNoCapturesC (fun cell ->
                    (
                        streamSink,
                        cell
                            |> mapC (Seq.map TestObject.output >> liftAllC Seq.sum)
                            |> switchCWithDeferredValues
                            |> filterS (flip (<) 50)
                            |> mapToS (List.append (List.singleton <| TestObject.create ()))
                            |> snapshotC cell (<|)
                    ) |> orElseS
                    |> holdS (List.init 10 (fun _ -> TestObject.create ())))
            let objectCounts = List<_>()
            objectCounts.Add -1
            let l = cell |> listenC (objectCounts.Add << List.length)
            objectCounts.Add -1
            (cell |> sampleC).[2].Input1 |> sendS 1
            objectCounts.Add -1
            (cell |> sampleC).[1].Input1 |> sendS -20
            objectCounts.Add -1
            streamSink |> sendS List.empty
            objectCounts.Add -1
            l |> unlistenL
            
            // Ideal result, likely not achievable.
            //CollectionAssert.AreEquivalent ([-1;10;-1;11;-1;15;-1;10;-1], objectCounts)

            // Glitchy result, but correct otherwise.
            CollectionAssert.AreEquivalent ([-1;10;-1;11;-1;12;13;14;15;-1;0;1;2;3;4;5;6;7;8;9;10;-1], objectCounts)