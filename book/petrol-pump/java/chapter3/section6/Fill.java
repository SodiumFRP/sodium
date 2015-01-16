package chapter3.section6;

import pump.*;
import chapter3.section5.AccumulatePulsesPump;
import sodium.*;
import java.util.Optional;

public class Fill {
    public final Cell<Double> price;
    public final Cell<Double> dollarsDelivered;
    public final Cell<Double> litersDelivered;

    public Fill(
            Stream<Unit> sClearAccumulator, Stream<Integer> sFuelPulses,
            Cell<Double> calibration, Cell<Double> price1,
            Cell<Double> price2, Cell<Double> price3,
            Stream<Fuel> sStart) {
        price = capturePrice(sStart, price1, price2, price3);
        litersDelivered = AccumulatePulsesPump.accumulate(
                sClearAccumulator, sFuelPulses, calibration);
        dollarsDelivered = Cell.lift(
                (liters, price_) -> liters * price_,
                litersDelivered, price);
    }

    public static Cell<Double> capturePrice(
            Stream<Fuel> sStart,
            Cell<Double> price1, Cell<Double> price2,
            Cell<Double> price3) {
        Stream<Optional<Double>> sPrice1 = sStart.snapshot(price1,
            (f, p) -> f == Fuel.ONE ? Optional.of(p)
                                    : Optional.empty());
        Stream<Optional<Double>> sPrice2 = sStart.snapshot(price2,
            (f, p) -> f == Fuel.TWO ? Optional.of(p)
                                    : Optional.empty());
        Stream<Optional<Double>> sPrice3 = sStart.snapshot(price3,
            (f, p) -> f == Fuel.THREE ? Optional.of(p)
                                      : Optional.empty());

        return Stream.filterOptional(
            sPrice1.merge(sPrice2.merge(sPrice3))
        ).hold(0.0);
    }
}
