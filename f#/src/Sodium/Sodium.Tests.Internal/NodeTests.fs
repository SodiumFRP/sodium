module Sodium.Tests.Internal.Node

open NUnit.Framework
open Sodium

[<TestFixture>]
type Tests() =

    [<Test>]
    member __.``Test Node``() =
        let a = Node<int>(0L)
        let b = Node<int>(1L)
        a.Link (fun t v -> ()) b |> ignore
        Assert.That(a, Is.LessThan(b))
