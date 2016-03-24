namespace Add

open System
open FSharpx.Functional.Prelude
open FsXaml
open Sodium
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let txtA = new STextBox("5", Width = 100.0)
        let txtB = new STextBox("10", Width = 100.0)

        let parseIntOrZero n = match Int32.parse n with | None -> 0 | Some n -> n
        let a = txtA.Text |> Cell.map parseIntOrZero
        let b = txtB.Text |> Cell.map parseIntOrZero
        let sum = Cell.lift2 (+) a b
        let lblSum = new SLabel(sum |> Cell.map string)

        view.Container.Children.Add(txtA) |> ignore
        view.Container.Children.Add(txtB) |> ignore
        view.Container.Children.Add(lblSum) |> ignore