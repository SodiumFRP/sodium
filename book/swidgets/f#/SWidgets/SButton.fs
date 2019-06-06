namespace SWidgets

open System
open System.Windows.Controls
open Sodium.Frp
open SWidgets.DispatcherExtensionMethods

type SButton(enabled) as this =
    inherit Button()
    let init () =
        let sClicked = sinkS ()
        this.Click.Subscribe (fun _ -> sClicked |> sendS ()) |> ignore
        postT (fun () -> this.IsEnabled <- enabled |> sampleC)
        let listener = (enabled |> updatesC |> listenS (fun e -> this.Dispatcher.InvokeIfNecessary (fun () -> this.IsEnabled <- e)))
        sClicked, listener
    let sClicked, listener = init ()

    new() = new SButton(constantC true)

    member val SClicked = sClicked

    interface IDisposable with
        member __.Dispose() = listener.Unlisten ()