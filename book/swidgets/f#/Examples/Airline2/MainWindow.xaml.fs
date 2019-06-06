namespace Airline2

open System
open FsXaml
open Sodium.Frp
open SWidgets

type MainWindowBase = XAML<"MainWindow.xaml">

type private Rule = { f : DateTime -> DateTime -> bool }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module private Rule =
    let reify r dep ret = (dep, ret) |> lift2C r.f
    let ``and`` r1 r2 = { f = (fun d r -> r1.f d r && r2.f d r) }

type MainWindow = 
    inherit MainWindowBase
    
    new () as this =
        { inherit MainWindowBase () }
        then
            let unlucky (d : DateTime) =
                let day = d.Day
                day = 4 || day = 14 || day = 24
            
            let dep = new SDateField()
            let ret = new SDateField()
            let r1 = { f = (<=) }
            let r2 = { f = (fun d r -> not (unlucky d) && not (unlucky r)) }
            let rule = Rule.``and`` r1 r2
            let valid = Rule.reify rule dep.SelectedDate ret.SelectedDate
            let ok = new SButton(valid, Content = "OK", Width = 75.0)
    
            this.DeparturePlaceholder.Children.Add(dep) |> ignore
            this.ReturnPlaceholder.Children.Add(ret) |> ignore
            this.ButtonPlaceholder.Children.Add(ok) |> ignore