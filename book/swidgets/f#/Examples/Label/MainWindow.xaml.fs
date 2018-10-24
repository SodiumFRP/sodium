namespace Label

open System.Windows
open FsXaml
open SWidgets

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow = 
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let msg = new STextBox("Hello", Width = 150.0)
            let lbl = new SLabel(msg.Text, Width = 150.0, Margin = Thickness(5.0, 0.0, 0.0, 0.0))
    
            this.Container.Children.Add(msg) |> ignore
            this.Container.Children.Add(lbl) |> ignore