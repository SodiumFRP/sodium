module SodiumFRP.Async

open System.Threading
open System.Threading.Tasks

let WithCancellation (ct : CancellationToken, a : Async<'a>) : Async<'a> = async {
    let! ct2 = Async.CancellationToken
    use cts = CancellationTokenSource.CreateLinkedTokenSource (ct, ct2)
    let tcs = new TaskCompletionSource<'a>()
    use _reg = cts.Token.Register (fun () -> tcs.TrySetCanceled() |> ignore)
    let a = async {
        try
            let! a = a
            tcs.TrySetResult a |> ignore
        with ex ->
            tcs.TrySetException ex |> ignore }
    Async.Start (a, cts.Token)
    return! tcs.Task |> Async.AwaitTask }

let StartAsVoidTask (a : Async<unit>) : Task = upcast (a |> Async.StartAsTask)