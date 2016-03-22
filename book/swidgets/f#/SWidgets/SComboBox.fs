namespace SWidgets

open System
open System.Windows.Controls
open Sodium

type SComboBox<'T>(setSelectedItem : 'T option Stream, initSelectedItem : 'T option, items : 'T seq) as this =
    inherit ComboBox()

    let init () =
        let setSelectedItemImpl m =
            this.BaseSelectedItem <-
                (match m with
                    | None -> null
                    | Some v -> box v)
        this.ItemsSource <- items
        let sDecrement = Stream.sink ()
        let allow = setSelectedItem |> Stream.map (fun _ -> 1) |> Stream.orElse sDecrement |> Stream.accum (+) 0 |> Cell.map ((=) 0)
        let getSelectedItem () =
            let sel = this.BaseSelectedItem
            if sel = null then Option.None else match (box sel) with | :? 'T as s -> Option.Some s | _ -> Option.None
        let sUserSelectedItem = Stream.sink ()
        let selectedItem = sUserSelectedItem |> Stream.gate allow |> Stream.orElse setSelectedItem |> Stream.hold initSelectedItem
        let subscribe () = 
            this.SelectionChanged.Subscribe (fun _ ->
                let selectedItem = getSelectedItem ()
                this.Dispatcher.InvokeAsync(fun () -> sUserSelectedItem.Send selectedItem) |> ignore)
        let mutable selectionChangedEventHandler = subscribe ()
        let listener = setSelectedItem |> Stream.listen (fun m ->
            this.Dispatcher.InvokeAsync(fun () ->
                selectionChangedEventHandler.Dispose()
                setSelectedItemImpl m
                selectionChangedEventHandler <- subscribe ()
                sDecrement.Send -1) |> ignore)
        sUserSelectedItem, selectedItem, (fun () -> listener.Unlisten ())

    let sUserSelectedItem, selectedItem, disposeListener = init ()

    member private __.BaseSelectedItem
        with get () = base.SelectedItem
        and set value = base.SelectedItem <- value

    member val SelectedItem = selectedItem
    member val SUserSelectedItem = sUserSelectedItem

    interface IDisposable with
        member __.Dispose() = disposeListener ()