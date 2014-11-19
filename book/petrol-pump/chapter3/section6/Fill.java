package chapter3.section6;

import pump.*;
import chapter3.section5.AccumulatePulsesPump;
import sodium.*;
import java.util.Optional;

public class Fill {
    public final Behavior<Double> price;
    public final Behavior<Double> dollarsDelivered;
    public final Behavior<Double> litersDelivered;

    public Fill(
            Event<Unit> eClearAccumulator, Event<Integer> eFuelPulses,
            Behavior<Double> calibration, Behavior<Double> price1,
            Behavior<Double> price2, Behavior<Double> price3,
            Event<Fuel> eStart) {
        price = capturePrice(eStart, price1, price2, price3);
        litersDelivered = AccumulatePulsesPump.accumulate(
                eClearAccumulator, eFuelPulses, calibration);
        dollarsDelivered = Behavior.lift(
                (liters, price_) -> liters * price_,
                litersDelivered, price);
    }

    public static Behavior<Double> capturePrice(
            Event<Fuel> eStart,
            Behavior<Double> price1, Behavior<Double> price2,
            Behavior<Double> price3) {
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
}
