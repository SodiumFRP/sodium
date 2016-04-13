namespace SWidgets

open System
open System.Windows
open System.Windows.Controls
open FSharpx.Functional.Prelude
open Sodium

type SSpinner private(initialValue : int) as this =
    inherit Grid()

    let loopSetValue sSetValue =
        let textField = new STextBox(sSetValue |> Stream.map string, string initialValue)
        textField.VerticalContentAlignment <- VerticalAlignment.Center
        let parseIntOrZero n = match Int32.parse n with | None -> 0 | Some n -> n
        let value = textField.Text |> Cell.map parseIntOrZero

        let plus = new SButton(Content = "+", Width = 25.0)
        let minus = new SButton(Content = "-", Width = 25.0)

        this.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
        this.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
        this.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength(1.0, GridUnitType.Star)))
        this.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Auto))

        Grid.SetRow(textField, 0)
        Grid.SetColumn(textField, 0)
        Grid.SetRowSpan(textField, 2)
        this.Children.Add(textField) |> ignore

        Grid.SetRow(plus, 0)
        Grid.SetColumn(plus, 1)
        this.Children.Add(plus) |> ignore

        Grid.SetRow(minus, 1)
        Grid.SetColumn(minus, 1)
        this.Children.Add(minus) |> ignore

        let sPlusDelta = plus.SClicked |> Stream.mapTo 1
        let sMinusDelta = minus.SClicked |> Stream.mapTo -1
        let sDelta = sPlusDelta |> Stream.orElse sMinusDelta
        let sSetValue = sDelta |> Stream.snapshot (+) value

        sSetValue, value

    let _, value = Stream.loop loopSetValue

    member val Value = value

    static member create initialValue = Transaction.Run (fun () -> SSpinner(initialValue))