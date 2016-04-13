namespace SWidgets

open System
open System.Windows.Controls
open Sodium
open SWidgets.DispatcherExtensionMethods

type STextBox(setText : string Stream, initText : string, enabled : bool Cell) as this =
    inherit TextBox()

    let mutable textChangedEventHandler = Option<IDisposable>.None

    let init () =
        let sDecrement = Stream.sink ()
        let allow = setText |> Stream.mapTo 1 |> Stream.orElse sDecrement |> Stream.accum (+) 0 |> Cell.map ((=) 0)
        Transaction.Post (fun () -> this.Dispatcher.InvokeIfNecessary (fun () -> this.IsEnabled <- enabled |> Cell.sample))
        let sUserChanges = Stream.sink ()
        let text = sUserChanges |> Stream.gate allow |> Stream.orElse setText |> Stream.hold initText
        let subscribe () = 
            this.TextChanged.Subscribe (fun _ ->
                let text = this.BaseText
                this.Dispatcher.InvokeAsync(fun () -> sUserChanges.Send text) |> ignore)
        let listener = setText |> Stream.listen (fun t ->
            this.Dispatcher.InvokeAsync(fun () ->
                match textChangedEventHandler with | None -> () | Some h -> h.Dispose()
                this.BaseText <- t
                textChangedEventHandler <- Option.Some (subscribe ())
                sDecrement.Send -1) |> ignore)
        let listener = Listener.append listener (enabled |> Operational.updates |> Stream.listen (fun e -> this.Dispatcher.InvokeIfNecessary(fun () -> this.IsEnabled <- e)))
        sUserChanges, text, (fun () -> listener.Unlisten ()), subscribe

    let sUserChanges, text, disposeListener, subscribe = init ()

    do
        base.Text <- initText
        textChangedEventHandler <- Option.Some (subscribe ())

    new(setText, initText) = new STextBox(setText, initText, Cell.constant true)
    new(initText, enabled) = new STextBox(Stream.never (), initText, enabled)
    new(initText) = new STextBox(Stream.never (), initText)

    member private __.BaseText
        with get () = base.Text
        and set value = base.Text <- value

    member val Text = text
    member val SUserChanges = sUserChanges

    interface IDisposable with
        member __.Dispose() = disposeListener ()