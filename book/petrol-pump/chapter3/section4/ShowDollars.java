package chapter3.section4;

import pump.*;
import chapter3.section2.LifeCycle;
import chapter3.section2.LifeCycle.LifeCycleOut;
import chapter3.section3.AccumulatePulses;
import sodium.*;
import java.util.Optional;

public class ShowDollars implements Pump
{
    public Outputs create(Inputs inputs)
    {
        LifeCycleOut lc = LifeCycle.lifeCycle(inputs.eNozzle1,
                                              inputs.eNozzle2,
                                              inputs.eNozzle3);

        FillOut fo = fill(lc.eStart.map(u -> Unit.UNIT),
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
            .setSaleCostLCD(fo.dollarsDelivered.map(
                q -> Formatters.formatSaleCost(q)))
            .setSaleQuantityLCD(fo.litersDelivered.map(
                q -> Formatters.formatSaleQuantity(q)))
            .setPriceLCD1(priceLCD(lc.fillActive, fo.price, Fuel.ONE,
                inputs))
            .setPriceLCD2(priceLCD(lc.fillActive, fo.price, Fuel.TWO,
                inputs))
            .setPriceLCD3(priceLCD(lc.fillActive, fo.price, Fuel.THREE,
                inputs));
    }

    public static class FillOut {
        public FillOut(Behavior<Double> price,
                       Behavior<Double> dollarsDelivered,
                       Behavior<Double> litersDelivered)
        {
            this.price = price;
            this.dollarsDelivered = dollarsDelivered;
            this.litersDelivered = litersDelivered;
        }
        public final Behavior<Double> price;
        public final Behavior<Double> dollarsDelivered;
        public final Behavior<Double> litersDelivered;
    }

    public static FillOut fill(
            Event<Unit> eClearAccumulator, Event<Integer> eFuelPulses,
            Behavior<Double> calibration, Behavior<Double> price1,
            Behavior<Double> price2, Behavior<Double> price3,
            Event<Fuel> eStart)
    {
        Behavior<Double> price = capturePrice(eStart,
                price1, price2, price3);

        Behavior<Double> litersDelivered = AccumulatePulses.accumulate(
                eClearAccumulator, eFuelPulses, calibration);

        Behavior<Double> dollarsDelivered = Behavior.lift(
                (liters, price_) -> liters * price_,
                litersDelivered, price);

        return new FillOut(price, dollarsDelivered, litersDelivered);
    }

    public static Behavior<Double> capturePrice(
            Event<Fuel> eStart,
            Behavior<Double> price1, Behavior<Double> price2,
            Behavior<Double> price3)
    {
        Event<Optional<Double>> ePrice1 = eStart.snapshot(price1,
            (f, p) -> f == Fuel.ONE ? Optional.of(p)
                                    : Optional.empty());
        Event<Optional<Double>> ePrice2 = eStart.snapshot(price2,
            (f, p) -> f == Fuel.TWO ? Optional.of(p)
                                    : Optional.empty());
        Event<Optional<Double>> ePrice3 = eStart.snapshot(price3,
            (f, p) -> f == Fuel.THREE ? Optional.of(p)
                                      : Optional.empty());

        return Event.filterOptional(
            ePrice1.merge(ePrice2.merge(ePrice3))
        ).hold(0.0);
    }

    public static Behavior<String> priceLCD(
            Behavior<Optional<Fuel>> fillActive,
            Behavior<Double> fillPrice,
            Fuel fuel,
            Inputs inputs)
    {
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

