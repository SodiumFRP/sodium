module Sodium.Tests.Common

open NUnit.Framework
open Sodium
open System.Collections.Generic

[<TestFixture>]
type Tests() =
        
    [<Test>]
    member __.``Test Base Send 1``() =
        use s = Stream.sink ()
        let out = List<_>()
        (
            use _l = s |> Stream.listen out.Add
            s.Send "a"
            s.Send "b"
        )
        CollectionAssert.AreEqual(["a";"b"], out)
            
    [<Test>]
    member __.``Test Operational Split``() =
        use a = Stream.sink ()
        use b = a |> Operational.split
        let out = List<_>()
        (
            use _l = b |> Stream.listen out.Add
            a.Send ["a";"b"]
        )
        CollectionAssert.AreEqual(["a";"b"], out)
                        
    [<Test>]
    member __.``Test Operational Defer 1``() =
        use a = Stream.sink ()
        use b = a |> Operational.defer
        let out = List<_>()
        (
            use _l = b |> Stream.listen out.Add
            a.Send "a"
        )
        CollectionAssert.AreEqual(["a"], out)
        let out2 = List<_>()
        (
            use _l = b |> Stream.listen out2.Add
            a.Send "b"
        )
        CollectionAssert.AreEqual(["b"], out2)
                                    
    [<Test>]
    member __.``Test Operational Defer 2``() =
        use a = Stream.sink ()
        use b = Stream.sink ()
        use c = a |> Operational.defer |> Stream.orElse b
        let out = List<_>()
        (
            use _l = c |> Stream.listen out.Add
            a.Send "a"
        )
        CollectionAssert.AreEqual(["a"], out)
        let out2 = List<_>()
        (
            use _l = c |> Stream.listen out2.Add
            Transaction.Run (fun () -> a.Send "b"; b.Send "B")
        )
        CollectionAssert.AreEqual(["B";"b"], out2)
                                                
    [<Test>]
    member __.``Test Stream OrElse 1``() =
        use a = Stream.sink ()
        use b = Stream.sink ()
        use c = a |> Stream.orElse b
        let out = List<_>()
        (
            use _l = c |> Stream.listen out.Add
            a.Send 0
        )
        CollectionAssert.AreEqual([0], out)
        let out2 = List<_>()
        (
            use _l = c |> Stream.listen out2.Add
            b.Send 10
        )
        CollectionAssert.AreEqual([10], out2)
        let out3 = List<_>()
        (
            use _l = c |> Stream.listen out3.Add
            Transaction.Run (fun () -> a.Send 2; b.Send 20)
        )
        CollectionAssert.AreEqual([2], out3)
        let out4 = List<_>()
        (
            use _l = c |> Stream.listen out4.Add
            b.Send 30
        )
        CollectionAssert.AreEqual([30], out4)

    [<Test>]
    member __.``Test Operational Defer Simultaneous``() =
        use a = Stream.sink ()
        use b = Stream.sink ()
        use c = a |> Operational.defer |> Stream.orElse (b |> Operational.defer)
        let out = List<_>()
        (
            use _l = c |> Stream.listen out.Add
            a.Send "A"
        )
        CollectionAssert.AreEqual(["A"], out)
        let out2 = List<_>()
        (
            use _l = c |> Stream.listen out2.Add
            Transaction.Run (fun () -> a.Send "b"; b.Send "B")
        )
        CollectionAssert.AreEqual(["b"], out2)