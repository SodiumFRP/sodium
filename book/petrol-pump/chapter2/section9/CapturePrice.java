package chapter2.section9;

import pump.*;
import sodium.*;

public class CapturePrice implements Pump
{
    public Outputs create(Inputs inputs) {
        return new Outputs()
            .setPriceLCD1(perFuel(inputs.eNozzle1, inputs.price1))
            .setPriceLCD2(perFuel(inputs.eNozzle2, inputs.price2))
            .setPriceLCD3(perFuel(inputs.eNozzle3, inputs.price3));
    }

    private static Behavior<String> perFuel(
                Event<UpDown> eNozzle, Behavior<Double> price) {
        Behavior<Double> capPrice = eNozzle.snapshot(price).hold(0.0);
        Behavior<UpDown> nozzle = eNozzle.hold(UpDown.DOWN);
        return Behavior.lift(
            (u, price_) ->
                u.equals(UpDown.UP) ? Formatters.formatPrice(price_)
                                    : "",
            nozzle, capPrice);
    }
}

