namespace Add

open System
open FsXaml
open Sodium.Frp
open SWidgets

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow =
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let txtA = new STextBox("5", Width = 100.0)
            let txtB = new STextBox("10", Width = 100.0)
    
            let parseIntOrZero n = match Int32.TryParse n with | false, _ -> 0 | true, n -> n
            let a = txtA.Text |> mapC parseIntOrZero
            let b = txtB.Text |> mapC parseIntOrZero
            let sum = (a, b) |> lift2C (+)
            let lblSum = new SLabel(sum |> mapC string)
    
            this.Container.Children.Add(txtA) |> ignore
            this.Container.Children.Add(txtB) |> ignore
            this.Container.Children.Add(lblSum) |> ignore