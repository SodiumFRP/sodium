namespace Flow

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
            let ok = FrButton.create (constantC "OK")
            let cancel = FrButton.create (constantC "Cancel")
            let dialog = FrFlow.create Orientation.Horizontal [ok;cancel]
            let lOk = FrButton.sClicked ok |> listenS (fun _ -> addMessage "OK")
            let lCancel = FrButton.sClicked cancel |> listenS (fun _ -> addMessage "Cancel")
            new FrView(this, dialog, Listener.fromSeq [lOk;lCancel]))) |> ignore