namespace Flow

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
            let ok = FrButton.create (Cell.constant "OK")
            let cancel = FrButton.create (Cell.constant "Cancel")
            let dialog = FrFlow.create Orientation.Horizontal [ok;cancel]
            let lOk = FrButton.sClicked ok |> Stream.listen (fun _ -> addMessage "OK")
            let lCancel = FrButton.sClicked cancel |> Stream.listen (fun _ -> addMessage "Cancel")
            new FrView(view.Root, dialog, Listener.fromSeq [lOk;lCancel]))) |> ignore