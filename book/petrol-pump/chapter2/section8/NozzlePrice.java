package chapter2.section8;

import pump.*;
import sodium.*;

public class NozzlePrice implements Pump
{
    public Outputs create(Inputs inputs) {
        Behavior<UpDown> nozzle1 = inputs.eNozzle1.hold(UpDown.DOWN);
        Behavior<UpDown> nozzle2 = inputs.eNozzle2.hold(UpDown.DOWN);
        Behavior<UpDown> nozzle3 = inputs.eNozzle3.hold(UpDown.DOWN);
        Lambda2<UpDown, Double, String> ifLiftedPrice =
            (u, price) ->
                u.equals(UpDown.UP) ? Formatters.formatPrice(price)
                                    : "";
        return new Outputs()
            .setPriceLCD1(
                Behavior.lift(ifLiftedPrice, nozzle1, inputs.price1))
            .setPriceLCD2(
                Behavior.lift(ifLiftedPrice, nozzle2, inputs.price2))
            .setPriceLCD3(
                Behavior.lift(ifLiftedPrice, nozzle3, inputs.price3));
    }
}
