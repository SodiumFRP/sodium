module Sodium.Frp.Tests.Transaction

open System.Collections.Generic
open NUnit.Framework
open Sodium.Frp
open System.Threading

[<TestFixture>]
type ``Transaction Tests``() =

    [<Test>]
    member __.``Run Construct``() =
        let out = List<_>()
        let struct (sink, l) = runT (fun () ->
            let sink = sinkS ()
            sink |> sendS 4
            let s = sink |> mapS ((*) 2)
            let l = s |> listenS out.Add
            struct (sink, l))
        sink |> sendS 5
        sink |> sendS 6
        sink |> sendS 7
        l |> unlistenL
        sink |> sendS 8
        CollectionAssert.AreEqual([8;10;12;14], out)

    [<Test>]
    member __.``Run Construct Ignores Other Transactions``() =
        async {
            let out = List<_>()
            let sink = sinkS ()
            let! a =
                async {
                    return runT (fun () ->
                        Thread.Sleep 500
                        sink |> sendS 4
                        let s = sink |> mapS ((*) 2)
                        let l2 = s |> listenS out.Add
                        Thread.Sleep 500
                        l2)
                } |> Async.StartChild
            do! Async.Sleep 250
            sink |> sendS 5
            do! Async.Sleep 500
            sink |> sendS 6
            let! l = a
            sink |> sendS 7
            l |> unlistenL
            sink |> sendS 8
            CollectionAssert.AreEqual([8;12;14], out)
        } |> Async.StartAsVoidTask

    [<Test>]
    member __.``Nested Run Construct``() =
        async {
            let out = List<_>()
            let sink = sinkS ()
            let! a =
                async {
                    return runT (fun () ->
                        Thread.Sleep 500
                        sink |> sendS 4
                        let s = runT (fun () -> sink |> mapS ((*) 2))
                        let l2 = s |> listenS out.Add
                        Thread.Sleep 500
                        l2)
                } |> Async.StartChild
            do! Async.Sleep 250
            sink |> sendS 5
            do! Async.Sleep 500
            sink |> sendS 6
            let! l = a
            sink |> sendS 7
            l |> unlistenL
            sink |> sendS 8
            CollectionAssert.AreEqual([8;12;14], out)
        } |> Async.StartAsVoidTask

    [<Test>]
    member __.``Post``() =
        let cell = runT (fun () ->
            let s = sinkS ()
            s |> sendS 2
            s |> holdS 1)
        let mutable value = 0
        Transaction.post (fun () -> value <- cell |> sampleC)
        Assert.AreEqual (2, value)

    [<Test>]
    member __.``Nested Post``() =
        let cell = runT (fun () ->
            let s = sinkS ()
            s |> sendS 2
            Transaction.post (fun () ->
                s |> sendS 3
                Transaction.post (fun () -> s |> sendS 5))
            Transaction.post (fun () -> s |> sendS 4)
            s |> holdS 1)
        Assert.AreEqual (5, cell |> sampleC)

    [<Test>]
    member __.``Post In Transaction``() =
        let mutable value = 0
        runT (fun () ->
            let s = sinkS ()
            s |> sendS 2
            let c = s |> holdS 1
            Transaction.post (fun () -> value <- c |> sampleC)
            Assert.AreEqual (0, value))
        Assert.AreEqual (2, value)

    [<Test>]
    member __.``Post In Nested Transaction``() =
        let mutable value = 0
        runT (fun () ->
            let s = sinkS ()
            s |> sendS 2
            runT (fun () ->
                let c = s |> holdS 1
                Transaction.post (fun () -> value <- c |> sampleC))
            Assert.AreEqual (0, value))
        Assert.AreEqual (2, value)

    [<Test>]
    member __.``Post In Construct Transaction``() =
        let mutable value = 0
        runT (fun () ->
            let s = sinkS ()
            s |> sendS 2
            let c = s |> holdS 1
            Transaction.post (fun () -> value <- c |> sampleC)
            Assert.AreEqual (0, value))
        Assert.AreEqual (2, value)

    [<Test>]
    member __.``Post In Nested Construct Transaction``() =
        let mutable value = 0
        runT (fun () ->
            let s = sinkS ()
            s |> sendS 2
            runT (fun () ->
                let c = s |> holdS 1
                Transaction.post (fun () -> value <- c |> sampleC))
            Assert.AreEqual (0, value))
        Assert.AreEqual (2, value)
    
    [<Test>]
    member __.``Is Active``() =
        let isActive = runT Transaction.isActive
        Assert.IsTrue isActive
    
    [<Test>]
    member __.``Is Not Active``() =
        let isActive = Transaction.isActive ()
        Assert.IsFalse isActive
    
    [<Test>]
    member __.``Is Not Active Separate Thread``() =
        let mutable threadIsActive1 = None
        let mutable threadIsActive2 = None
        let mutable threadIsActive3 = None
        let mutable threadIsActive4 = None
        let mutable threadIsActive5 = None
        (Thread (fun () ->
            threadIsActive1 <- Some (Transaction.isActive ())
            Thread.Sleep 500
            threadIsActive2 <- Some (Transaction.isActive ())
            runT (fun () ->
                threadIsActive3 <- Some (Transaction.isActive ())
                Thread.Sleep 500
                threadIsActive4 <- Some (Transaction.isActive ()))
            threadIsActive5 <- Some (Transaction.isActive ()))).Start ()
        Thread.Sleep 250
        let isActive1 = Transaction.isActive ()
        Thread.Sleep 500
        let isActive2 = Transaction.isActive ()
        Thread.Sleep 500
        let isActive3 = Transaction.isActive ()

        Assert.IsFalse isActive1
        Assert.IsFalse isActive2
        Assert.IsFalse isActive3

        let getAssertIsFalseValue = function | Some v -> v | None -> true
        let getAssertIsTrueValue = function | Some v -> v | None -> false

        Assert.IsFalse (getAssertIsFalseValue threadIsActive1)
        Assert.IsFalse (getAssertIsFalseValue threadIsActive2)
        Assert.IsTrue (getAssertIsTrueValue threadIsActive3)
        Assert.IsTrue (getAssertIsTrueValue threadIsActive4)
        Assert.IsFalse (getAssertIsFalseValue threadIsActive5)