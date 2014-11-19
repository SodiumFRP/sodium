package chapter2.section12;

import pump.*;
import sodium.*;

public class CapturePriceFiltered implements Pump
{
    public Outputs create(Inputs inputs) {
        return new Outputs()
            .setPriceLCD1(perFuel(inputs.eNozzle1, inputs.price1))
            .setPriceLCD2(perFuel(inputs.eNozzle2, inputs.price2))
            .setPriceLCD3(perFuel(inputs.eNozzle3, inputs.price3));
    }

    enum StartFill { START_FILL };

    private static Behavior<String> perFuel(
                Event<UpDown> eNozzle, Behavior<Double> price) {
        Event<StartFill> eStartFill =
                               eNozzle.filter(u -> u == UpDown.UP)
                                      .map(u -> StartFill.START_FILL);
        Behavior<Double> capPrice = eStartFill.snapshot(price).hold(0.0);
        Behavior<UpDown> nozzle = eNozzle.hold(UpDown.DOWN);
        return Behavior.lift(
            (u, price_) ->
                u.equals(UpDown.UP) ? Formatters.formatPrice(price_)
                                    : "",
            nozzle, capPrice);
    }
}

