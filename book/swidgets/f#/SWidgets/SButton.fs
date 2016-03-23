namespace SWidgets

open System
open System.Windows.Controls
open Sodium
open SWidgets.DispatcherExtensionMethods

type SButton(enabled : bool Cell) as this =
    inherit Button()
    let init () =
        let sClicked = Stream.sink ()
        this.Click.Subscribe (fun _ -> sClicked.Send ()) |> ignore
        Transaction.Post(fun () -> this.IsEnabled <- enabled |> Cell.sample)
        let listener = (enabled |> Operational.updates |> Stream.listen (fun e -> this.Dispatcher.InvokeIfNecessary (fun () -> this.IsEnabled <- e)))
        sClicked, listener
    let sClicked, listener = init ()

    new() = new SButton(Cell.constant true)

    member val SClicked = sClicked

    interface IDisposable with
        member __.Dispose() = listener.Unlisten ()