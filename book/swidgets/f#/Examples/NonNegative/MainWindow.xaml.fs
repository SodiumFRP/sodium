namespace NonNegative

open System.Windows
open FSharpx.Functional.Prelude
open FsXaml
open Sodium
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        Cell.loopWithNoCaptures (fun value ->
            let lblValue = new SLabel(value |> Cell.map string)
            let plus = new SButton(Content = "+", Width = 25.0, Margin = Thickness(5.0, 0.0, 0.0, 0.0))
            let minus = new SButton(Content = "-", Width = 25.0, Margin = Thickness(5.0, 0.0, 0.0, 0.0))

            view.Container.Children.Add(lblValue) |> ignore
            view.Container.Children.Add(plus) |> ignore
            view.Container.Children.Add(minus) |> ignore

            let sPlusDelta = plus.SClicked |> Stream.mapTo 1
            let sMinusDelta = minus.SClicked |> Stream.mapTo -1
            let sDelta = sPlusDelta |> Stream.orElse sMinusDelta
            let sUpdate = sDelta |> Stream.snapshot (+) value |> Stream.filter (flip (>=) 0)
            sUpdate |> Stream.hold 0
        ) |> ignore