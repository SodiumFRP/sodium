namespace Airline1

open FsXaml
open Sodium.Frp
open SWidgets

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow =
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let dep = new SDateField()
            let ret = new SDateField()
            let valid = (dep.SelectedDate, ret.SelectedDate) |> lift2C (<=)
            let ok = new SButton(valid, Content = "OK", Width = 75.0)
    
            this.DeparturePlaceholder.Children.Add(dep) |> ignore
            this.ReturnPlaceholder.Children.Add(ret) |> ignore
            this.ButtonPlaceholder.Children.Add(ok) |> ignore