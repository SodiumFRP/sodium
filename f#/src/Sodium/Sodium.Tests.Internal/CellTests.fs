module Sodium.Tests.Internal.Cell

open NUnit.Framework
open Sodium

[<TestFixture>]
type Tests() =

    [<Test>]
    member __.``Test Transaction``() =
        let mutable calledBack = false
        Transaction.Apply (fun t -> t.Prioritized Node<Unit>.Null (fun _ -> calledBack <- true))
        Assert.IsTrue(calledBack)
