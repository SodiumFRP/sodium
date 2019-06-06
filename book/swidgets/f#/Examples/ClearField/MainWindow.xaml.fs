namespace ClearField

open FsXaml
open Sodium.Frp
open SWidgets

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow = 
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let clear = new SButton(Content = "Clear", Width = 75.0)
            let sClearIt = clear.SClicked |> mapToS ""
            let text = new STextBox(sClearIt, "Hello", Width = 100.0)
    
            this.Container.Children.Add(text) |> ignore
            this.Container.Children.Add(clear) |> ignore