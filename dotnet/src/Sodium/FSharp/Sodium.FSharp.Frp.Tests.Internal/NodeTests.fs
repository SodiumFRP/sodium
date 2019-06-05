module Sodium.Frp.Tests.Internal.Node

open NUnit.Framework
open Sodium.Frp

[<TestFixture>]
type ``Node Tests``() =

    [<Test>]
    member __.``Test Node``() =
        let a = Node<int> ()
        let b = Node<int> ()
        TransactionInternal.Apply
            (
                (fun trans _ ->
                    a.Link (trans, (fun _ _ -> ()), b) |> ignore
                    trans.Prioritized (a, (fun _ -> ()))),
                false
            )
        Assert.That (a.Rank, Is.LessThan b.Rank)
