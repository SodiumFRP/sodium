namespace SWidgets

open System
open System.Windows.Controls
open Sodium.Frp
open SWidgets.DispatcherExtensionMethods

type SLabel(text : Cell<string>) as this =
    inherit TextBlock()

    let init () =
        let setText t = this.Dispatcher.InvokeIfNecessary (fun () -> this.Text <- t)
        postT (fun () -> setText (text |> sampleC))
        let listener = text |> updatesC |> listenS setText
        listener

    let listener = init ()

    interface IDisposable with
        member __.Dispose() = listener.Unlisten ()