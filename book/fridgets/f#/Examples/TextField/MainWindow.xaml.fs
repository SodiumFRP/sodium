namespace TextField

open System.Windows.Controls
open Fridgets
open FsXaml
open Sodium

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() =
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        
        let addMessage message =
            view.StackPanel.Children.Add(TextBlock(Text = message)) |> ignore
            view.ScrollViewer.ScrollToBottom()

        view.Container.Children.Add(Transaction.Run (fun () ->
            let firstName = FrTextField.create "Joe"
            let lastName = FrTextField.create "Bloggs"
            let ok = FrButton.create (Cell.constant "OK")
            let cancel = FrButton.create (Cell.constant "Cancel")
            let buttonPanel = FrFlow.create Orientation.Horizontal [ok;cancel]
            let dialog = FrFlow.create Orientation.Vertical [buttonPanel :> IFridget;firstName :> IFridget;lastName :> IFridget]
            let getFullName firstName lastName = firstName + " " + lastName
            let getFullName' _ firstName lastName = getFullName firstName lastName
            let lOk = FrButton.sClicked ok |> Stream.snapshot2 getFullName' (FrTextField.text firstName) (FrTextField.text lastName) |> Stream.listen (fun n -> addMessage (sprintf "OK: %s" n))
            let lCancel = FrButton.sClicked cancel |> Stream.listen (fun _ -> addMessage "Cancel")
            new FrView(view.Root, dialog, Listener.fromSeq [lOk;lCancel]))) |> ignore