namespace SWidgets

open System
open System.Windows.Controls
open Sodium.Frp

type SComboBox<'T>(setSelectedItem : Stream<'T option>, initSelectedItem : 'T option, items : 'T seq) as this =
    inherit ComboBox()

    let mutable selectionChangedEventHandler : IDisposable option = None

    let getObjectFromSelectedItem m =
        (match m with
            | None -> null
            | Some v -> box v)

    let init () =
        let sDecrement = sinkS ()
        let allow = (setSelectedItem |> mapToS 1, sDecrement) |> orElseS |> accumS 0 (+) |> mapC ((=) 0)
        let getSelectedItem () =
            let sel = this.BaseSelectedItem
            if sel = null then None else match (box sel) with | :? 'T as s -> Some s | _ -> None
        let sUserSelectedItem = sinkS ()
        let selectedItem = (sUserSelectedItem |> Stream.gate allow, setSelectedItem) |> orElseS |> holdS initSelectedItem
        let subscribe () = 
            this.SelectionChanged.Subscribe (fun _ ->
                let selectedItem = getSelectedItem ()
                this.Dispatcher.InvokeAsync(fun () -> sUserSelectedItem |> sendS selectedItem) |> ignore)
        let listener = setSelectedItem |> Stream.listen (fun o ->
            this.Dispatcher.InvokeAsync(fun () ->
                match selectionChangedEventHandler with | None -> () | Some h -> h.Dispose()
                this.BaseSelectedItem <- getObjectFromSelectedItem o
                selectionChangedEventHandler <- Option.Some (subscribe ())
                sDecrement |> sendS -1) |> ignore)
        sUserSelectedItem, selectedItem, subscribe, (fun () -> listener.Unlisten ())

    let sUserSelectedItem, selectedItem, subscribe, disposeListener = init ()

    do
        base.ItemsSource <- items
        base.SelectedItem <- getObjectFromSelectedItem initSelectedItem
        selectionChangedEventHandler <- Option.Some (subscribe ())

    new(setSelectedItem, initSelectedItem) = new SComboBox<_>(setSelectedItem, initSelectedItem, Seq.empty)
    new(initSelectedItem, items) = new SComboBox<_>(Stream.never (), initSelectedItem, items)
    new(initSelectedItem) = new SComboBox<_>(Stream.never (), initSelectedItem)
    new(items) = new SComboBox<_>(Stream.never (), Option.None, items)
    new() = new SComboBox<_>(Seq.empty)

    member private __.BaseSelectedItem
        with get () = base.SelectedItem
        and set value = base.SelectedItem <- value

    member val SelectedItem = selectedItem
    member val SUserSelectedItem = sUserSelectedItem

    interface IDisposable with
        member __.Dispose() = disposeListener ()