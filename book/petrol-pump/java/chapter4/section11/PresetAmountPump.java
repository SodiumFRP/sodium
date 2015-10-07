package chapter4.section11;

import pump.*;
import chapter4.section4.LifeCycle;
import chapter4.section4.LifeCycle.End;
import chapter4.section6.AccumulatePulsesPump;
import chapter4.section7.Fill;
import chapter4.section7.ShowDollarsPump;
import chapter4.section8.NotifyPointOfSale;
import chapter4.section9.Keypad;
import nz.sodium.*;
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
            .setBeep(np.sBeep.orElse(ke.sBeep));
    }
}

