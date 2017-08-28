namespace PetrolPump

open Sodium

type Sale =
    {
        fuel : Fuel
        price : float
        cost : float
        quantity : float
    }

type Inputs =
    {
        sNozzle1 : UpDown Stream
        sNozzle2 : UpDown Stream
        sNozzle3 : UpDown Stream
        sKeypad : Key Stream
        sFuelPulses : int Stream
        calibration : float Cell
        price1 : float Cell
        price2 : float Cell
        price3 : float Cell
        sClearSale : unit Stream
    }

type Outputs =
    {
        delivery : Delivery Cell
        presetLcd : string Cell
        saleCostLcd : string Cell
        saleQuantityLcd : string Cell
        priceLcd1 : string Cell
        priceLcd2 : string Cell
        priceLcd3 : string Cell
        sBeep : unit Stream
        sSaleComplete : Sale Stream
    }

type IPump =
    abstract member Create : Inputs -> Outputs