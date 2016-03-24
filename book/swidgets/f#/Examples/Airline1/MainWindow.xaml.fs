namespace Airline1

open FsXaml
open Sodium
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() = 
    inherit WindowViewController<MainView>()

    override __.OnLoaded view =
        let dep = new SDateField()
        let ret = new SDateField()
        let valid = Cell.lift2 (<=) dep.SelectedDate ret.SelectedDate
        let ok = new SButton(valid, Content = "OK", Width = 75.0)

        view.DeparturePlaceholder.Children.Add(dep) |> ignore
        view.ReturnPlaceholder.Children.Add(ret) |> ignore
        view.ButtonPlaceholder.Children.Add(ok) |> ignore