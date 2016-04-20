namespace Supply

open System.Windows.Controls
open Fridgets
open FsXaml

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() =
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        
        let addMessage message =
            view.StackPanel.Children.Add(TextBlock(Text = message)) |> ignore
            view.ScrollViewer.ScrollToBottom()

        let a = Supply.create ()
        let b = Supply.child1 a
        let c = Supply.child1 b
        let bAgain = Supply.child1 a
        addMessage (Supply.get a |> (sprintf "a = %i"))
        addMessage (Supply.get b |> (sprintf "b = %i"))
        addMessage (Supply.get c |> (sprintf "c = %i"))
        addMessage (Supply.get bAgain |> (sprintf "bAgain = %i"))