namespace Sodium.Tests

open NUnit.Framework
open Sodium
open System.Collections.Generic

[<TestFixture>]
type StreamTests() = 

    [<Test>]
    member __.``Test Stream Send``() =
        use s = Stream.sink ()
        let out = List<_>()
        (
            use l = (s |> Stream.listen out.Add)
            s.Send 5
        )
        CollectionAssert.AreEqual([5], out)
        s.Send 6
        CollectionAssert.AreEqual([5], out)

    [<Test>]
    member __.``Test Map``() =
        use s = Stream.sink ()
        use m = s |> Stream.map ((+) 2 >> string)
        let out = List<_>()
        (
            use l = (m |> Stream.listen out.Add)
            s.Send 5
            s.Send 3
        )
        CollectionAssert.AreEqual(["7"; "5"], out)
