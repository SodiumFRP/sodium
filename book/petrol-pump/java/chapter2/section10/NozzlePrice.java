package chapter2.section10;

import pump.*;
import sodium.*;

public class NozzlePrice implements Pump
{
    public Outputs create(Inputs inputs) {
        Cell<UpDown> nozzle1 = inputs.sNozzle1.hold(UpDown.DOWN);
        Cell<UpDown> nozzle2 = inputs.sNozzle2.hold(UpDown.DOWN);
        Cell<UpDown> nozzle3 = inputs.sNozzle3.hold(UpDown.DOWN);
        Lambda2<UpDown, Double, String> ifLiftedPrice =
            (u, price) ->
                u.equals(UpDown.UP) ? Formatters.formatPrice(price)
                                    : "";
        return new Outputs()
            .setPriceLCD1(
                Cell.lift(ifLiftedPrice, nozzle1, inputs.price1))
            .setPriceLCD2(
                Cell.lift(ifLiftedPrice, nozzle2, inputs.price2))
            .setPriceLCD3(
                Cell.lift(ifLiftedPrice, nozzle3, inputs.price3));
    }
}
