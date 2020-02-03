module Sodium.Frp.Tests.Internal.Transaction

open System
open NUnit.Framework
open Sodium.Frp
open System.Threading

[<TestFixture>]
type ``Transaction Tests``() =

    [<Test>]
    member __.``Post See Outside``() =
        async {
            use re = new AutoResetEvent false
            let! actual =
                async {
                    use cts = new CancellationTokenSource ()
                    let! a =
                        async {
                            Transaction.post (fun () ->
                                re.Set () |> ignore
                                Thread.Sleep 5000
                                cts.Token.ThrowIfCancellationRequested ())
                        } |> Async.StartChild
                    re.WaitOne () |> ignore
                    cts.Cancel ()
                    try
                        do! a
                        return None
                    with
                        | :? OperationCanceledException as e -> return Some e
                }
            Assert.IsTrue (Option.isSome actual)
        } |> Async.StartAsVoidTask

    [<Test>]
    member __.``Post See Inside``() =
        async {
            use re = new AutoResetEvent false
            let! actual =
                async {
                    use cts = new CancellationTokenSource ()
                    let! a =
                        async {
                            Transaction.post (fun () ->
                                re.Set () |> ignore
                                Thread.Sleep 5000
                                cts.Token.ThrowIfCancellationRequested ())
                        } |> Async.StartChild
                    re.WaitOne () |> ignore
                    let sink2 = sinkS ()
                    let _ = (
                        use _l = sink2 |> listenS (fun _ -> cts.Cancel ())
                        sink2 |> sendS ()
                    )
                    try
                        do! a
                        return None
                    with
                        | :? OperationCanceledException as e -> return Some e
                }
            Assert.IsTrue (Option.isSome actual)
        } |> Async.StartAsVoidTask