module Sodium.Frp.Tests.Common

open System.Collections.Generic
open NUnit.Framework
open Sodium.Frp

[<TestFixture>]
type ``Common Tests``() =

    [<Test>]
    member __.``Test Base Send 1``() =
         let s = sinkS ()
         let out = List<_>()
         let l = s |> listenS out.Add
         s |> sendS "a"
         s |> sendS "b"
         l |> unlistenL
         CollectionAssert.AreEqual (["a";"b"], out)

    [<Test>]
    member __.``Test Operational Split``() =
        let a = sinkS ()
        let b = a |> Operational.split
        let out = List<_>()
        let l = b |> listenS out.Add
        a |> sendS [|"a";"b"|]
        l |> unlistenL
        CollectionAssert.AreEqual (["a";"b"], out)

    [<Test>]
    member __.``Test Operational Defer 1``() =
        let a = sinkS ()
        let b = a |> Operational.defer
        let out = List<_>()
        let l = b |> listenS out.Add
        a |> sendS "a"
        l |> unlistenL
        CollectionAssert.AreEqual (["a"], out)
        let out = List<_>()
        let l = b |> listenS out.Add
        a |> sendS "b"
        l |> unlistenL
        CollectionAssert.AreEqual (["b"], out)

    [<Test>]
    member __.``Test Operational Defer 2``() =
        let a = sinkS ()
        let b = sinkS ()
        let c = (a |> Operational.defer, b) |> orElseS
        let out = List<_>()
        let l = c |> listenS out.Add
        a |> sendS "a"
        l |> unlistenL
        CollectionAssert.AreEqual (["a"], out)
        let out = List<_>()
        let l = c |> listenS out.Add
        runT (fun () ->
            a |> sendS "b"
            b |> sendS "B")
        l |> unlistenL
        CollectionAssert.AreEqual (["B";"b"], out)

    [<Test>]
    member __.``Test Stream OrElse 1``() =
        let a = sinkS ()
        let b = sinkS ()
        let c = (a, b) |> orElseS
        let out = List<_>()
        let l = c |> listenS out.Add
        a |> sendS 0
        l |> unlistenL
        CollectionAssert.AreEqual ([0], out)
        let out = List<_>()
        let l = c |> listenS out.Add
        b |> sendS 10
        l |> unlistenL
        CollectionAssert.AreEqual ([10], out)
        let out = List<_>()
        let l = c |> listenS out.Add
        runT (fun () ->
            a |> sendS 2
            b |> sendS 20)
        l |> unlistenL
        CollectionAssert.AreEqual ([2], out)
        let out = List<_>()
        let l = c |> listenS out.Add
        b |> sendS 30
        l |> unlistenL
        CollectionAssert.AreEqual ([30], out)

    [<Test>]
    member __.``Test Operational Defer Simultaneous``() =
        let a = sinkS ()
        let b = sinkS ()
        let c = (a |> Operational.defer, b |> Operational.defer) |> orElseS
        let out = List<_>()
        let l = c |> listenS out.Add
        a |> sendS "A"
        l |> unlistenL
        CollectionAssert.AreEqual (["A"], out)
        let out = List<_>()
        let l = c |> listenS out.Add
        runT (fun () ->
            a |> sendS "b"
            b |> sendS "B")
        l |> unlistenL
        CollectionAssert.AreEqual (["b"], out)