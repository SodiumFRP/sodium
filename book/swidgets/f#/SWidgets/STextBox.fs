namespace SWidgets

open System
open System.Windows.Controls
open Sodium.Frp
open SWidgets.DispatcherExtensionMethods

type STextBox(setText : Stream<string>, initText : string, enabled : Cell<bool>) as this =
    inherit TextBox()

    let mutable textChangedEventHandler = Option<IDisposable>.None

    let init () =
        let sDecrement = sinkS ()
        let allow = (setText |> mapToS 1, sDecrement) |> orElseS |> accumS 0 (+) |> mapC ((=) 0)
        postT (fun () -> this.Dispatcher.InvokeIfNecessary (fun () -> this.IsEnabled <- enabled |> sampleC))
        let sUserChanges = sinkS ()
        let text = (sUserChanges |> gateC allow, setText) |> orElseS |> holdS initText
        let subscribe () = 
            this.TextChanged.Subscribe (fun _ ->
                let text = this.BaseText
                this.Dispatcher.InvokeAsync(fun () -> sUserChanges |> sendS text) |> ignore)
        let listener = setText |> listenS (fun t ->
            this.Dispatcher.InvokeAsync(fun () ->
                match textChangedEventHandler with | None -> () | Some h -> h.Dispose()
                this.BaseText <- t
                textChangedEventHandler <- Some (subscribe ())
                sDecrement |> sendS -1) |> ignore)
        let listener = Listener.append listener (enabled |> updatesC |> listenS (fun e -> this.Dispatcher.InvokeIfNecessary(fun () -> this.IsEnabled <- e)))
        sUserChanges, text, (fun () -> listener |> unlistenL), subscribe

    let sUserChanges, text, disposeListener, subscribe = init ()

    do
        base.Text <- initText
        textChangedEventHandler <- Some (subscribe ())

    new(setText, initText) = new STextBox(setText, initText, constantC true)
    new(initText, enabled) = new STextBox(neverS (), initText, enabled)
    new(initText) = new STextBox(neverS (), initText)

    member private __.BaseText
        with get () = base.Text
        and set value = base.Text <- value

    member val Text = text
    member val SUserChanges = sUserChanges

    interface IDisposable with
        member __.Dispose() = disposeListener ()