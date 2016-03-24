namespace SWidgets

open System
open System.Windows.Controls
open FSharpx.Option
open Sodium

type SDateField() as this =
    inherit WrapPanel()

    static let months = [("Jan",1);("Feb",2);("Mar",3);("Apr",4);("May",5);("Jun",6);("Jul",7);("Aug",8);("Sep",9);("Oct",10);("Nov",11);("Dec",12)]
    static let monthNumberByName = months |> Map.ofList
    static let monthNameByNumber = months |> List.map (fun (x,y) -> (y,x)) |> Map.ofList

    let init () =
        let now = DateTime.Now
        let year = new SComboBox<_>(Option.Some now.Year, Seq.init 21 ((+) (now.Year - 10)))
        let month = new SComboBox<_>(Option.Some (monthNameByNumber |> Map.find now.Month), months |> List.map fst)
        let day = new SComboBox<_>(Option.Some now.Day, Seq.init 31 ((+) 1))
        this.Children.Add(year) |> ignore
        this.Children.Add(month) |> ignore
        this.Children.Add(day) |> ignore
        let monthIndex = month.SelectedItem |> Cell.map (fun o -> match o with | None -> Option.None | Some s -> Option.Some (monthNumberByName |> Map.find s))
        let getSelectedDate y m d = maybe {
            let! y = y
            let! m = m
            let! d = d
            return DateTime(y, m, d)
        }
        let dateOrNow d = match d with | None -> DateTime.Now | Some d -> d
        let selectedDate = Cell.lift3 getSelectedDate year.SelectedItem monthIndex day.SelectedItem |> Cell.map dateOrNow
        selectedDate

    let selectedDate = init ()

    member val SelectedDate = selectedDate
