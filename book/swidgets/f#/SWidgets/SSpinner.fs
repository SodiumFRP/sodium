namespace SWidgets

open System
open System.Windows
open System.Windows.Controls
open Sodium.Frp

type SSpinner private(initialValue : int) as this =
    inherit Grid()

    let struct (_, value) = loopS (fun sSetValue ->
        let textField = new STextBox(sSetValue |> mapS string, string initialValue)
        textField.VerticalContentAlignment <- VerticalAlignment.Center
        let parseIntOrZero n = match Int32.TryParse n with | false, _ -> 0 | true, n -> n
        let value = textField.Text |> mapC parseIntOrZero

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

        let sPlusDelta = plus.SClicked |> mapToS 1
        let sMinusDelta = minus.SClicked |> mapToS -1
        let sDelta = (sPlusDelta, sMinusDelta) |> orElseS
        let sSetValue = sDelta |> snapshotC value (+)

        struct (sSetValue, value))

    member val Value = value

    static member create initialValue = runT (fun () -> SSpinner(initialValue))