namespace Supply

open System.Windows.Controls
open Fridgets
open FsXaml

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() =
    inherit MainWindowBase()

    override this.OnLoaded (_, _) =
        
        let addMessage message =
            this.StackPanel.Children.Add(TextBlock(Text = message)) |> ignore
            this.ScrollViewer.ScrollToBottom()

        let a = Supply.create ()
        let b = Supply.child1 a
        let c = Supply.child1 b
        let bAgain = Supply.child1 a
        addMessage (Supply.get a |> (sprintf "a = %i"))
        addMessage (Supply.get b |> (sprintf "b = %i"))
        addMessage (Supply.get c |> (sprintf "c = %i"))
        addMessage (Supply.get bAgain |> (sprintf "bAgain = %i"))