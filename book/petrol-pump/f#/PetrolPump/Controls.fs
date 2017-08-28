namespace PetrolPump

open System.Collections
open System.Windows.Controls
open Sodium

type SButton() as this =
    inherit Button()

    let init () =
        let sClickedSink = Stream.sink ()
        this.Click.Subscribe (fun _ -> Async.Start (async { sClickedSink.Send () })) |> ignore
        sClickedSink :> unit Stream

    let sClicked = init ()

    member val SClicked = sClicked

type 'T SComboBox(items : 'T seq, getDisplayName : 'T -> string) as this =
    inherit ComboBox()

    let init () =
        let createComboBoxItem item = ComboBoxItem(Content = getDisplayName item, Tag = item)
        this.ItemsSource <- List.ofSeq (items |> Seq.map createComboBoxItem) :> IEnumerable
        this.SelectedIndex <- 0
        let getSelectedItem () = (box this.BaseSelectedItem :?> ComboBoxItem).Tag :?> 'T
        let selectedItem = Stream.sink ()
        this.SelectionChanged.Subscribe (fun _ -> Async.Start (async { selectedItem.Send (getSelectedItem ()) })) |> ignore
        selectedItem |> Stream.holdLazy (lazy (getSelectedItem ()))

    let selectedItem = init ()

    member val SelectedItem = selectedItem

    member __.BaseSelectedItem = base.SelectedItem

type STextField(initialText : string) as this =
    inherit TextBox()

    let init () =
        let text = Cell.sink initialText
        let subscribe = (fun () -> this.TextChanged.Subscribe (fun _ ->
            let t = this.BaseText
            Async.Start (async { text.Send t })))
        text, subscribe

    let text, subscribe = init ()

    do
        base.Text <- initialText
        subscribe () |> ignore

    member val Text = text

    member __.BaseText = base.Text