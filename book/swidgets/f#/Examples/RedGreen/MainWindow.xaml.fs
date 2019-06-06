namespace RedGreen

open System.Windows
open FsXaml
open Sodium.Frp
open SWidgets

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow = 
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let red = new SButton(Content = "red", Width = 75.0)
            let green = new SButton(Content = "green", Width = 75.0)
            let sRed = red.SClicked |> Stream.mapTo "red"
            let sGreen = green.SClicked |> Stream.mapTo "green"
            let sColor = (sRed, sGreen) |> orElseS
            let color = sColor |> Stream.hold ""
            let lbl = new SLabel(color, Width = 75.0, Margin = Thickness(5.0, 0.0, 0.0, 0.0))
    
            this.Container.Children.Add(red) |> ignore
            this.Container.Children.Add(green) |> ignore
            this.Container.Children.Add(lbl) |> ignore