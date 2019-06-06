namespace TextField

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
            let firstName = FrTextField.create "Joe"
            let lastName = FrTextField.create "Bloggs"
            let ok = FrButton.create (constantC "OK")
            let cancel = FrButton.create (constantC "Cancel")
            let buttonPanel = FrFlow.create Orientation.Horizontal [ok;cancel]
            let dialog = FrFlow.create Orientation.Vertical [buttonPanel :> IFridget;firstName :> IFridget;lastName :> IFridget]
            let getFullName firstName lastName = firstName + " " + lastName
            let getFullName' _ firstName lastName = getFullName firstName lastName
            let lOk = FrButton.sClicked ok |> snapshot2C (FrTextField.text firstName) (FrTextField.text lastName) getFullName' |> listenS (fun n -> addMessage (sprintf "OK: %s" n))
            let lCancel = FrButton.sClicked cancel |> listenS (fun _ -> addMessage "Cancel")
            new FrView(this, dialog, Listener.fromSeq [lOk;lCancel]))) |> ignore