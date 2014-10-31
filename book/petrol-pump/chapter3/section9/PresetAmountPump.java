package chapter3.section9;

import pump.*;
import chapter3.section2.LifeCycle;
import chapter3.section2.LifeCycle.End;
import chapter3.section4.AccumulatePulsesPump;
import chapter3.section5.Fill;
import chapter3.section5.ShowDollarsPump;
import chapter3.section6.NotifyPointOfSale;
import chapter3.section7.Keypad;
import sodium.*;
import java.util.Optional;

public class PresetAmountPump implements Pump {
    public Outputs create(Inputs inputs) {
        EventLoop<Fuel> eStart = new EventLoop<>();
        Fill fi = new Fill(inputs.eClearSale.map(u -> Unit.UNIT),
                           inputs.eFuelPulses, inputs.calibration,
                           inputs.price1, inputs.price2, inputs.price3,
                           eStart);
        NotifyPointOfSale np = new NotifyPointOfSale(
                new LifeCycle(inputs.eNozzle1,
                              inputs.eNozzle2,
                              inputs.eNozzle3),
                inputs.eClearSale,
                fi);
        eStart.loop(np.eStart);
        BehaviorLoop<Boolean> keypadActive = new BehaviorLoop<>();
        Keypad ke = new Keypad(inputs.eKeypad,
                               inputs.eClearSale,
                               keypadActive);
        Preset pr = new Preset(ke.value,
                               fi,
                               np.fuelFlowing,
                               np.fillActive.map(o -> o.isPresent()));
        keypadActive.loop(pr.keypadActive);
        return new Outputs()
            .setDelivery(pr.delivery)
            .setSaleCostLCD(fi.dollarsDelivered.map(
                    q -> Formatters.formatSaleCost(q)))
            .setSaleQuantityLCD(fi.litersDelivered.map(
                    q -> Formatters.formatSaleQuantity(q)))
            .setPriceLCD1(ShowDollarsPump.priceLCD(np.fillActive, fi.price,
                    Fuel.ONE, inputs))
            .setPriceLCD2(ShowDollarsPump.priceLCD(np.fillActive, fi.price,
                    Fuel.TWO, inputs))
            .setPriceLCD3(ShowDollarsPump.priceLCD(np.fillActive, fi.price,
                    Fuel.THREE, inputs))
            .setSaleComplete(np.eSaleComplete)
            .setPresetLCD(ke.value.map(v ->
                Formatters.formatSaleCost((double)v)))
            .setBeep(np.eBeep.merge(ke.eBeep));
    }
}

