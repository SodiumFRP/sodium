module Sodium.Frp.Tests.Internal.Cell

open System.Collections.Generic
open NUnit.Framework
open Sodium.Frp

[<TestFixture>]
type ``Cell Tests``() =

    [<Test>]
    member __.``Test Transaction``() =
        let mutable calledBack = false
        TransactionInternal.Apply
            (
                (fun trans _ ->
                    trans.Prioritized (Node<unit>.Null, fun trans -> calledBack <- true)),
                false
            )
        Assert.IsTrue calledBack
    
    [<Test>]
    member __.``Test Regen``() =
        let out = List<_>()
        TransactionInternal.Apply
            (
                (fun trans _ ->
                    let setNeedsRegeneratingAndPrioritized a =
                        trans.SetNeedsRegenerating ()
                        trans.Prioritized (Node<unit> (), (fun _ -> a ()))
                    setNeedsRegeneratingAndPrioritized (fun () -> out.Add 1)
                    setNeedsRegeneratingAndPrioritized (fun () ->
                        setNeedsRegeneratingAndPrioritized (fun () -> out.Add 4))
                    setNeedsRegeneratingAndPrioritized (fun () -> out.Add 2)
                    setNeedsRegeneratingAndPrioritized (fun () ->
                        setNeedsRegeneratingAndPrioritized (fun () ->
                            setNeedsRegeneratingAndPrioritized (fun () -> out.Add 6)))
                    setNeedsRegeneratingAndPrioritized (fun () ->
                        setNeedsRegeneratingAndPrioritized (fun () -> out.Add 5))
                    trans.Prioritized (Node<unit> (), fun _ -> out.Add 3)),
                false
            )
        CollectionAssert.AreEqual ([1;2;3;4;5;6], out)