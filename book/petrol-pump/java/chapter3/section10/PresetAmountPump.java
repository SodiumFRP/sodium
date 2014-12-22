package chapter3.section10;

import pump.*;
import chapter3.section3.LifeCycle;
import chapter3.section3.LifeCycle.End;
import chapter3.section5.AccumulatePulsesPump;
import chapter3.section6.Fill;
import chapter3.section6.ShowDollarsPump;
import chapter3.section7.NotifyPointOfSale;
import chapter3.section8.Keypad;
import sodium.*;
import java.util.Optional;

public class PresetAmountPump implements Pump {
    public Outputs create(Inputs inputs) {
        StreamLoop<Fuel> sStart = new StreamLoop<>();
        Fill fi = new Fill(inputs.sClearSale.map(u -> Unit.UNIT),
                           inputs.sFuelPulses, inputs.calibration,
                           inputs.price1, inputs.price2, inputs.price3,
                           sStart);
        NotifyPointOfSale np = new NotifyPointOfSale(
                new LifeCycle(inputs.sNozzle1,
                              inputs.sNozzle2,
                              inputs.sNozzle3),
                inputs.sClearSale,
                fi);
        sStart.loop(np.sStart);
        CellLoop<Boolean> keypadActive = new CellLoop<>();
        Keypad ke = new Keypad(inputs.sKeypad,
                               inputs.sClearSale,
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
            .setSaleComplete(np.sSaleComplete)
            .setPresetLCD(ke.value.map(v ->
                Formatters.formatSaleCost((double)v)))
            .setBeep(np.sBeep.merge(ke.sBeep));
    }
}

