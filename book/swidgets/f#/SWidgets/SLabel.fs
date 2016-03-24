namespace SWidgets

open System
open System.Windows.Controls
open Sodium
open SWidgets.DispatcherExtensionMethods

type SLabel(text : string Cell) as this =
    inherit TextBlock()

    let init () =
        let setText t = this.Dispatcher.InvokeIfNecessary (fun () -> this.Text <- t)
        Transaction.Post (fun () -> setText (text |> Cell.sample))
        let listener = text |> Operational.updates |> Stream.listen setText
        listener

    let listener = init ()

    interface IDisposable with
        member __.Dispose() = listener.Unlisten ()