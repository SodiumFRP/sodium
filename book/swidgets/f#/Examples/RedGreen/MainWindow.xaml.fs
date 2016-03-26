namespace RedGreen

open System.Windows
open FsXaml
open Sodium
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let red = new SButton(Content = "red", Width = 75.0)
        let green = new SButton(Content = "green", Width = 75.0)
        let sRed = red.SClicked |> Stream.mapTo "red"
        let sGreen = green.SClicked |> Stream.mapTo "green"
        let sColor = sRed |> Stream.orElse sGreen
        let color = sColor |> Stream.hold ""
        let lbl = new SLabel(color, Width = 75.0, Margin = Thickness(5.0, 0.0, 0.0, 0.0))

        view.Container.Children.Add(red) |> ignore
        view.Container.Children.Add(green) |> ignore
        view.Container.Children.Add(lbl) |> ignore