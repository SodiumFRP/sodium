namespace Button

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
            let b = FrButton.create (Cell.constant "OK")
            let l = FrButton.sClicked b |> Stream.listen (fun _ -> addMessage "clicked!")
            new FrView(view.Root, b, l))) |> ignore