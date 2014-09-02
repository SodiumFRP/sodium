package chapter3.section7;

import pump.*;
import chapter3.section2.LifeCycle;
import chapter3.section2.LifeCycle.End;
import chapter3.section2.LifeCycle.LifeCycleOut;
import chapter3.section3.AccumulatePulses;
import chapter3.section4.ShowDollars;
import chapter3.section4.ShowDollars.FillOut;
import chapter3.section5.ClearSale;
import chapter3.section5.ClearSale.NotifyPOSOut;
import chapter3.section6.Keypad;
import chapter3.section6.Keypad.KeypadOut;
import sodium.*;
import java.util.Optional;

public class PresetAmount implements Pump
{
    public Outputs create(Inputs inputs)
    {
        EventLoop<Fuel> eStart = new EventLoop<>();

        FillOut fo = ShowDollars.fill(
                          inputs.eClearSale.map(u -> Unit.UNIT),
                          inputs.eFuelPulses, inputs.calibration,
                          inputs.price1, inputs.price2, inputs.price3,
                          eStart);

        NotifyPOSOut np = ClearSale.notifyPointOfSale(
                LifeCycle.lifeCycle(inputs.eNozzle1,
                                    inputs.eNozzle2,
                                    inputs.eNozzle3),
                inputs.eClearSale,
                fo);
        eStart.loop(np.eStart);

        KeypadOut ko = Keypad.keypad(inputs.eKeypad, new Event<Unit>());

        return new Outputs()
            .setDelivery(np.filling.map(
                of ->
                    of.equals(Optional.of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.equals(Optional.of(Fuel.TWO))   ? Delivery.FAST2 :
                    of.equals(Optional.of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF))
            .setSaleCostLCD(fo.dollarsDelivered.map(
                    q -> Formatters.formatSaleCost(q)))
            .setSaleQuantityLCD(fo.litersDelivered.map(
                    q -> Formatters.formatSaleQuantity(q)))
            .setPriceLCD1(ShowDollars.priceLCD(np.fuelSelected, fo.price,
                    Fuel.ONE, inputs))
            .setPriceLCD2(ShowDollars.priceLCD(np.fuelSelected, fo.price,
                    Fuel.TWO, inputs))
            .setPriceLCD3(ShowDollars.priceLCD(np.fuelSelected, fo.price,
                    Fuel.THREE, inputs))
            .setSaleComplete(np.eSaleComplete)
            .setPresetLCD(ko.value.map(v ->
                Formatters.formatSaleCost((double)v)))
            .setBeep(np.eBeep.merge(ko.eBeep));
    }
}

