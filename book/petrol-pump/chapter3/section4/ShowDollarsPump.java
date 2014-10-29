package chapter3.section4;

import pump.*;
import chapter3.section2.LifeCycle;
import sodium.*;
import java.util.Optional;

public class ShowDollarsPump implements Pump {
    public Outputs create(Inputs inputs) {
        LifeCycle lc = new LifeCycle(inputs.eNozzle1,
                                     inputs.eNozzle2,
                                     inputs.eNozzle3);
        Fill fi = new Fill(lc.eStart.map(u -> Unit.UNIT),
                           inputs.eFuelPulses, inputs.calibration,
                           inputs.price1, inputs.price2, inputs.price3,
                           lc.eStart);
        return new Outputs()
            .setDelivery(lc.fillActive.map(
                of ->
                    of.equals(Optional.of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.equals(Optional.of(Fuel.TWO))   ? Delivery.FAST2 :
                    of.equals(Optional.of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF))
            .setSaleCostLCD(fi.dollarsDelivered.map(
                q -> Formatters.formatSaleCost(q)))
            .setSaleQuantityLCD(fi.litersDelivered.map(
                q -> Formatters.formatSaleQuantity(q)))
            .setPriceLCD1(priceLCD(lc.fillActive, fi.price, Fuel.ONE,
                inputs))
            .setPriceLCD2(priceLCD(lc.fillActive, fi.price, Fuel.TWO,
                inputs))
            .setPriceLCD3(priceLCD(lc.fillActive, fi.price, Fuel.THREE,
                inputs));
    }

    public static Behavior<String> priceLCD(
            Behavior<Optional<Fuel>> fillActive,
            Behavior<Double> fillPrice,
            Fuel fuel,
            Inputs inputs) {
        Behavior<Double> idlePrice;
        switch (fuel) {
            case ONE:   idlePrice = inputs.price1; break;
            case TWO:   idlePrice = inputs.price2; break;
            case THREE: idlePrice = inputs.price3; break;
            default:    idlePrice = null;
        }
        return Behavior.lift((oFuelSelected, fillPrice_, idlePrice_) ->
            oFuelSelected.isPresent()
                ? oFuelSelected.get() == fuel
                                      ? Formatters.formatPrice(fillPrice_)
                                      : ""
                : Formatters.formatPrice(idlePrice_),
            fillActive,
            fillPrice,
            idlePrice);
    }
}

