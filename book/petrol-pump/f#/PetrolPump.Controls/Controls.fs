namespace PetrolPump.Controls

open System.Collections
open System.Windows.Controls
open Sodium.Frp

type SButton() as this =
    inherit Button()

    let init () : Stream<unit> =
        let sClickedSink = sinkS ()
        this.Click.Subscribe (fun _ -> Async.Start (async { sClickedSink |> sendS () })) |> ignore
        upcast sClickedSink

    let sClicked = init ()

    member val SClicked = sClicked

type SComboBox<'T> =
    inherit ComboBox
    
    val selectedItem : Cell<'T>
    
    new (items : 'T seq, getDisplayName : 'T -> string) as this =
        let cs = sinkC (constantC Unchecked.defaultof<'T>)
        { inherit ComboBox (); selectedItem = cs |> switchC }
        then
            let createComboBoxItem item = ComboBoxItem(Content = getDisplayName item, Tag = item)
            this.ItemsSource <- List.ofSeq (items |> Seq.map createComboBoxItem) :> IEnumerable
            this.SelectedIndex <- 0
            let getSelectedItem () = (box this.BaseSelectedItem :?> ComboBoxItem).Tag :?> 'T
            let selectedItem = sinkS ()
            this.SelectionChanged.Subscribe (fun _ ->
                let s = getSelectedItem ()
                Async.Start (async { selectedItem |> sendS (s) })) |> ignore
            cs |> sendC (selectedItem |> holdLazyS (lazy (getSelectedItem ())))

    member this.SelectedItem = this.selectedItem

    member __.BaseSelectedItem = base.SelectedItem

type STextField(initialText : string) as this =
    inherit TextBox()

    let init () =
        let text = sinkC initialText
        let subscribe = (fun () -> this.TextChanged.Subscribe (fun _ ->
            let t = this.BaseText
            async { text |> sendC t } |> Async.Start))
        text, subscribe

    let text, subscribe = init ()

    do
        base.Text <- initialText
        subscribe () |> ignore

    member val Text = text

    member __.BaseText = base.Text