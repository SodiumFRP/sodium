namespace PetrolPump

open System.Windows.Threading
open Sodium.Frp

module Formatters =
    let formatPrice price maxDigits =
        match maxDigits with
            | None -> sprintf "%0.3f" price
            | Some maxDigits ->
                let priceIntString = string (int64 (floor price))
                let decimalDigits = min (max (maxDigits - priceIntString.Length) 0) 3
                let f = Printf.StringFormat<float -> string>("%0." + (string decimalDigits) + "f")
                sprintf f price

    let formatSaleCost cost = sprintf "%0.2f" cost
    let formatSaleQuantity quantity = sprintf "%0.2f" quantity
    let formatPresetAmount presetAmount = formatSaleCost (float presetAmount)

module Cell =
    open System.Collections.Generic

    let changes cell =
        let areEqual n o = if EqualityComparer<_>.Default.Equals(o, n) then Option.None else Option.Some(n)
        cell |> valuesC |> snapshotC cell areEqual |> filterOptionS

module DispatcherExtensionMethods =
    type Dispatcher with
        member this.InvokeIfNecessary action =
            if this.CheckAccess() then action () else this.Invoke(action)
