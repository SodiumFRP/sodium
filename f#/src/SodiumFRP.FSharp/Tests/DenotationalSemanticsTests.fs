module SodiumFRP.FSharp.Tests.DenotationalSemantics

open System.Collections.Generic
open NUnit.Framework
open SodiumFRP.FSharp
open SodiumFRP.FSharp.Sodium

[<TestFixture>]
type Tests() =

    [<Test>]
    member __.``Test Base Send 1``() =
        ()