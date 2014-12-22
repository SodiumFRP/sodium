package chapter2.section11;

import pump.*;
import sodium.*;

public class CapturePrice implements Pump
{
    public Outputs create(Inputs inputs) {
        return new Outputs()
            .setPriceLCD1(perFuel(inputs.sNozzle1, inputs.price1))
            .setPriceLCD2(perFuel(inputs.sNozzle2, inputs.price2))
            .setPriceLCD3(perFuel(inputs.sNozzle3, inputs.price3));
    }

    private static Cell<String> perFuel(
                Stream<UpDown> sNozzle, Cell<Double> price) {
        Cell<Double> capPrice = sNozzle.snapshot(price).hold(0.0);
        Cell<UpDown> nozzle = sNozzle.hold(UpDown.DOWN);
        return Cell.lift(
            (u, price_) ->
                u.equals(UpDown.UP) ? Formatters.formatPrice(price_)
                                    : "",
            nozzle, capPrice);
    }
}

