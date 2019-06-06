namespace Button

open System.Windows.Controls
open Fridgets
open FsXaml
open Sodium.Frp

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() =
    inherit MainWindowBase()

    override this.OnLoaded (_, _) =
        
        let addMessage message =
            this.StackPanel.Children.Add(TextBlock(Text = message)) |> ignore
            this.ScrollViewer.ScrollToBottom()

        this.Container.Children.Add(runT (fun () ->
            let b = FrButton.create (constantC "OK")
            let l = FrButton.sClicked b |> listenS (fun _ -> addMessage "clicked!")
            new FrView(this, b, l))) |> ignore