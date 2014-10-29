package chapter3.section5;

import pump.*;
import chapter3.section2.LifeCycle;
import chapter3.section4.Fill;
import chapter3.section4.ShowDollarsPump;
import sodium.*;
import java.util.Optional;

public class ClearSalePump implements Pump {
    public Outputs create(Inputs inputs) {
        EventLoop<Fuel> eStart = new EventLoop<>();
        Fill fi = new Fill(
                          inputs.eClearSale.map(u -> Unit.UNIT),
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
        return new Outputs()
            .setDelivery(np.fuelFlowing.map(
                of ->
                    of.equals(Optional.of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.equals(Optional.of(Fuel.TWO))   ? Delivery.FAST2 :
                    of.equals(Optional.of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF))
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
            .setBeep(np.eBeep)
            .setSaleComplete(np.eSaleComplete);
    }
}

