namespace Airline2

open System
open FsXaml
open Sodium
open SWidgets

type MainView = XAML<"MainWindow.xaml", true>

type private Rule = { f : DateTime -> DateTime -> bool }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module private Rule =
    let reify r dep ret = Cell.lift2 r.f dep ret
    let ``and`` r1 r2 = { f = (fun d r -> r1.f d r && r2.f d r) }

type MainViewController() = 
    inherit WindowViewController<MainView>()

    let unlucky (d : DateTime) =
        let day = d.Day
        day = 4 || day = 14 || day = 24

    override __.OnLoaded view =
        let dep = new SDateField()
        let ret = new SDateField()
        let r1 = { f = (<=) }
        let r2 = { f = (fun d r -> not (unlucky d) && not (unlucky r)) }
        let rule = Rule.``and`` r1 r2
        let valid = Rule.reify rule dep.SelectedDate ret.SelectedDate
        let ok = new SButton(valid, Content = "OK", Width = 75.0)

        view.DeparturePlaceholder.Children.Add(dep) |> ignore
        view.ReturnPlaceholder.Children.Add(ret) |> ignore
        view.ButtonPlaceholder.Children.Add(ok) |> ignore