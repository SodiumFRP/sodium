namespace PetrolPump

open Sodium.Frp

type Sale =
    {
        fuel : Fuel
        price : float
        cost : float
        quantity : float
    }

type Inputs =
    {
        sNozzle1 : Stream<UpDown>
        sNozzle2 : Stream<UpDown>
        sNozzle3 : Stream<UpDown>
        sKeypad : Stream<Key>
        sFuelPulses : Stream<int>
        calibration : Cell<float>
        price1 : Cell<float>
        price2 : Cell<float>
        price3 : Cell<float>
        sClearSale : Stream<unit>
    }

type Outputs =
    {
        delivery : Cell<Delivery>
        presetLcd : Cell<string>
        saleCostLcd : Cell<string>
        saleQuantityLcd : Cell<string>
        priceLcd1 : Cell<string>
        priceLcd2 : Cell<string>
        priceLcd3 : Cell<string>
        sBeep : Stream<unit>
        sSaleComplete : Stream<Sale>
    }

module Outputs =
    let empty =
        {
            delivery = constantC Off;
            presetLcd = constantC "";
            saleCostLcd = constantC "";
            saleQuantityLcd = constantC "";
            priceLcd1 = constantC "";
            priceLcd2 = constantC "";
            priceLcd3 = constantC "";
            sBeep = neverS ();
            sSaleComplete = neverS ()
        }

type IPump =
    abstract member Create : Inputs -> Outputs

module Chapter4 =
    
    let accumulate sClearAccumulator sPulses calibration =
        let total = loopWithNoCapturesC (fun total ->
            (sClearAccumulator |> mapToS 0, sPulses |> snapshotC total (+)) |> orElseS |> holdS 0)
        (total, calibration) |> lift2C (fun total calibration -> float total * calibration)
    
    let priceLcd fillActive fillPrice fuel inputs =
        let idlePrice =
            match fuel with
            | Fuel.One -> inputs.price1
            | Fuel.Two -> inputs.price2
            | Fuel.Three -> inputs.price3
        (fillActive, fillPrice, idlePrice) |> lift3C (fun fillActive fillPrice idlePrice ->
            match fillActive with
            | Some f -> if f = fuel then Formatters.formatPrice fillPrice (Some 4) else ""
            | None -> Formatters.formatPrice idlePrice (Some 4))
    
    type private End = End

    type private LifeCycle (sNozzle1, sNozzle2, sNozzle3) =
        let whenLifted sNozzle nozzleFuel = sNozzle |> filterS (fun u -> u = Up) |> mapToS nozzleFuel
        let whenSetDown sNozzle nozzleFuel fillActive =
            sNozzle |> snapshotC fillActive (fun u f ->
                match u with
                | Down -> if f = Some nozzleFuel then Some End else None
                | Up -> None) |> filterOptionS
        let sLiftNozzle = ((whenLifted sNozzle1 Fuel.One, whenLifted sNozzle2 Fuel.Two) |> orElseS, whenLifted sNozzle3 Fuel.Three) |> orElseS
        let struct (fillActive, (sStart, sEnd)) = loopC (fun fillActive ->
            let sStart = sLiftNozzle |> snapshotC fillActive (fun newFuel fillActive -> match fillActive with | Some _ -> None | None -> Some newFuel) |> filterOptionS
            let sEnd = ((whenSetDown sNozzle1 Fuel.One fillActive, whenSetDown sNozzle2 Fuel.Two fillActive) |> orElseS, whenSetDown sNozzle3 Fuel.Three fillActive) |> orElseS
            let fillActive = (sEnd |> mapToS None, sStart |> mapS Some) |> orElseS |> holdS None
            struct (fillActive, (sStart, sEnd)))
        
        member __.FillActive = fillActive
        member __.SStart = sStart
        member __.SEnd = sEnd
    
    let getDelivery fuelFlowing =
        fuelFlowing |> mapC (fun fuelFlowing ->
            match fuelFlowing with 
            | Some Fuel.One -> Fast1
            | Some Fuel.Two -> Fast2
            | Some Fuel.Three -> Fast3
            | _ -> Off)
    
    type private Fill (sClearAccumulator, sFuelPulses, calibration, price1 : Cell<float>, price2 : Cell<float>, price3 : Cell<float>, sStart : Stream<Fuel>) =
        let getPriceStream c m = sStart |> snapshotC c (fun f p -> if f = m then Some p else None) |> filterOptionS
        let sPrice1 = getPriceStream price1 Fuel.One
        let sPrice2 = getPriceStream price2 Fuel.Two
        let sPrice3 = getPriceStream price3 Fuel.Three
        let price = ((sPrice1, sPrice2) |> orElseS, sPrice3) |> orElseS |> holdS 0.0
        let litersDelivered = accumulate sClearAccumulator sFuelPulses calibration
        let dollarsDelivered = (litersDelivered, price) |> lift2C (*)
        
        member __.Price = price
        member __.LitersDelivered = litersDelivered
        member __.DollarsDelivered = dollarsDelivered
    
    type private NotifyPointOfSale (lc : LifeCycle, sClearSale, fi : Fill) =
        let locked = (lc.SStart |> mapToS true, sClearSale |> mapToS false) |> orElseS |> holdS false
        let sStart = lc.SStart |> gateC (locked |> mapC not)
        let sEnd = lc.SEnd |> gateC locked
        let fuelFlowing = (sStart |> mapS Some, sEnd |> mapToS None) |> orElseS |> holdS None
        let fillActive = (sStart |> mapS Some, sClearSale |> mapToS None) |> orElseS |> holdS None
        let sBeep = sClearSale
        let sale =
            (fuelFlowing, fi.Price, fi.DollarsDelivered, fi.LitersDelivered)
                |> lift4C (fun fuel price dollars liters ->
                    fuel |> Option.map (fun v -> { fuel = v; price = price; cost = dollars; quantity = liters }))
        let sSaleComplete = sEnd |> snapshotAndTakeC sale |> filterOptionS
        
        member __.SStart = sStart
        member __.FillActive = fillActive
        member __.FuelFlowing = fuelFlowing
        member __.SEnd = sEnd
        member __.SBeep = sBeep
        member __.SSaleComplete = sSaleComplete
    
    type private Keypad (sKeypad, sClear) =
        let struct (value, sKeyUpdate) = loopC (fun value ->
            let sKeyUpdate = sKeypad |> snapshotC value (fun key value ->
                match key with
                | Clear -> Some 0
                | _ ->
                    let x10 = value * 10
                    if x10 >= 1000 then None
                    else
                        Some (
                            match key with
                            | Clear -> 0
                            | Zero -> x10
                            | One -> x10 + 1
                            | Two -> x10 + 2
                            | Three -> x10 + 3
                            | Four -> x10 + 4
                            | Five -> x10 + 5
                            | Six -> x10 + 6
                            | Seven -> x10 + 7
                            | Eight -> x10 + 8
                            | Nine -> x10 + 9)) |> filterOptionS
            struct ((sKeyUpdate, sClear |> mapToS 0) |> orElseS |> holdS 0, sKeyUpdate))
        let sBeep = sKeyUpdate |> mapToS ()
        
        new (sKeypad, sClear, active) = Keypad (sKeypad |> gateC active, sClear)
        
        member __.Value = value
        member __.SBeep = sBeep

    module Section4 =
        
        type LifeCyclePump() =
            interface IPump with
                member __.Create inputs =
                    let lc = LifeCycle (inputs.sNozzle1, inputs.sNozzle2, inputs.sNozzle3)
                    { Outputs.empty with
                        delivery = getDelivery lc.FillActive;
                        saleQuantityLcd =
                            lc.FillActive |> mapC (fun fillActive ->
                                match fillActive with
                                | Some Fuel.One -> "1"
                                | Some Fuel.Two -> "2"
                                | Some Fuel.Three -> "3"
                                | _ -> "") }

    module Section6 =
        
        type AccumulatePulsesPump() =
            interface IPump with
                member __.Create inputs =
                    let lc = LifeCycle (inputs.sNozzle1, inputs.sNozzle2, inputs.sNozzle3)
                    { Outputs.empty with
                        delivery = getDelivery lc.FillActive;
                        saleQuantityLcd =
                            accumulate (lc.SStart |> mapToS ()) inputs.sFuelPulses inputs.calibration |> mapC Formatters.formatSaleQuantity }
        
    module Section7 =
        
        type ShowDollarsPump() =
            interface IPump with
                member __.Create inputs =
                    let lc = LifeCycle (inputs.sNozzle1, inputs.sNozzle2, inputs.sNozzle3)
                    let fi = Fill (lc.SStart |> mapToS (), inputs.sFuelPulses, inputs.calibration, inputs.price1, inputs.price2, inputs.price3, lc.SStart)
                    { Outputs.empty with
                        delivery = getDelivery lc.FillActive;
                        saleCostLcd = fi.DollarsDelivered |> mapC Formatters.formatSaleCost;
                        saleQuantityLcd = fi.LitersDelivered |> mapC Formatters.formatSaleQuantity;
                        priceLcd1 = priceLcd lc.FillActive fi.Price Fuel.One inputs;
                        priceLcd2 = priceLcd lc.FillActive fi.Price Fuel.Two inputs;
                        priceLcd3 = priceLcd lc.FillActive fi.Price Fuel.Three inputs; }
        
    module Section8 =
        
        type ClearSalePump() =
            interface IPump with
                member __.Create inputs =
                    let struct (sStart, (np, fi)) = loopS (fun sStart ->
                        let fi = Fill (inputs.sClearSale, inputs.sFuelPulses, inputs.calibration, inputs.price1, inputs.price2, inputs.price3, sStart)
                        let lc = LifeCycle (inputs.sNozzle1, inputs.sNozzle2, inputs.sNozzle3)
                        let np = NotifyPointOfSale (lc, inputs.sClearSale, fi)
                        struct (np.SStart, (np, fi)))
                    { Outputs.empty with
                        delivery = getDelivery np.FuelFlowing;
                        saleCostLcd = fi.DollarsDelivered |> mapC Formatters.formatSaleCost;
                        saleQuantityLcd = fi.LitersDelivered |> mapC Formatters.formatSaleQuantity;
                        priceLcd1 = priceLcd np.FillActive fi.Price Fuel.One inputs;
                        priceLcd2 = priceLcd np.FillActive fi.Price Fuel.Two inputs;
                        priceLcd3 = priceLcd np.FillActive fi.Price Fuel.Three inputs;
                        sBeep = np.SBeep;
                        sSaleComplete = np.SSaleComplete }
        
    module Section9 =
        
        type KeypadPump() =
            interface IPump with
                member __.Create inputs =
                    let ke = Keypad (inputs.sKeypad, neverS ())
                    { Outputs.empty with
                        presetLcd = ke.Value |> mapC Formatters.formatPresetAmount;
                        sBeep = ke.SBeep }
        
    module Section11 =
        
        type private Speed = Fast | Slow | Stopped
        
        type private Preset(presetDollars, fi : Fill, fuelFlowing) =
            let speed =
                (presetDollars, fi.Price, fi.DollarsDelivered, fi.LitersDelivered)
                    |> lift4C (fun presetDollars price dollarsDelivered litersDelivered ->
                        if presetDollars = 0 then Fast
                        else if dollarsDelivered >= float presetDollars then Stopped
                        else
                            let slowLiters = float presetDollars / price - 0.1
                            if litersDelivered >= slowLiters then Slow else Fast)
            let delivery =
                (fuelFlowing, speed) |> lift2C (fun fuelFlowing speed ->
                    match speed with
                    | Fast -> match fuelFlowing with | Some Fuel.One -> Fast1 | Some Fuel.Two -> Fast2 | Some Fuel.Three -> Fast3 | None -> Off
                    | Slow -> match fuelFlowing with | Some Fuel.One -> Slow1 | Some Fuel.Two -> Slow2 | Some Fuel.Three -> Slow3 | None -> Off
                    | Stopped -> Off)
            let keypadActive = (fuelFlowing, speed) |> lift2C (fun fuelFlowing speed ->
                match fuelFlowing with | Some _ -> speed = Fast | None -> true)
            
            member __.Delivery = delivery
            member __.KeypadActive = keypadActive
        
        type PresetAmountPump() =
            interface IPump with
                member __.Create inputs =
                    let struct (sStart, (np, fi)) = loopS (fun sStart ->
                        let fi = Fill (inputs.sClearSale, inputs.sFuelPulses, inputs.calibration, inputs.price1, inputs.price2, inputs.price3, sStart)
                        let lc = LifeCycle (inputs.sNozzle1, inputs.sNozzle2, inputs.sNozzle3)
                        let np = NotifyPointOfSale (lc, inputs.sClearSale, fi)
                        struct (np.SStart, (np, fi)))
                    let struct (keypadActive, (ke, pr)) = loopC (fun keypadActive ->
                        let ke = Keypad (inputs.sKeypad, inputs.sClearSale, keypadActive)
                        let pr = Preset (ke.Value, fi, np.FuelFlowing)
                        struct (pr.KeypadActive, (ke, pr)))
                    {
                        delivery = pr.Delivery;
                        saleCostLcd = fi.DollarsDelivered |> mapC Formatters.formatSaleCost;
                        saleQuantityLcd = fi.LitersDelivered |> mapC Formatters.formatSaleQuantity;
                        priceLcd1 = priceLcd np.FillActive fi.Price Fuel.One inputs;
                        priceLcd2 = priceLcd np.FillActive fi.Price Fuel.Two inputs;
                        priceLcd3 = priceLcd np.FillActive fi.Price Fuel.Three inputs;
                        sSaleComplete = np.SSaleComplete;
                        presetLcd = ke.Value |> mapC Formatters.formatPresetAmount;
                        sBeep = (np.SBeep, ke.SBeep) |> orElseS
                    }