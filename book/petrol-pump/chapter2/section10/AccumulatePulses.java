package chapter2.section10;

import pump.*;
import sodium.*;

public class AccumulatePulses implements Pump
{
    public Outputs create(Inputs inputs)
    {
        Behavior<UpDown> nozzle1 = inputs.eNozzle1.hold(UpDown.DOWN);
        Behavior<UpDown> nozzle2 = inputs.eNozzle2.hold(UpDown.DOWN);
        Behavior<UpDown> nozzle3 = inputs.eNozzle3.hold(UpDown.DOWN);
        Behavior<UpDown> anyNozzle = Behavior.lift((n1, n2, n3) ->
            n1 == UpDown.UP || n2 == UpDown.UP || n3 == UpDown.UP
                    ? UpDown.UP : UpDown.DOWN,
            nozzle1, nozzle2, nozzle3);

        BehaviorLoop<Integer> pulses = new BehaviorLoop<>();
        pulses.loop(inputs.eFuelPulses.snapshot(pulses,
            (neu, sum) -> neu + sum).hold(0));

        Behavior<Double> quantity = Behavior.lift(
            (pulses_, calibration_) -> pulses_ * calibration_,
            pulses, inputs.calibration);

        return new Outputs()
            .setDelivery(anyNozzle.map(
                    u -> u == UpDown.UP ? Delivery.FAST1
                                        : Delivery.OFF))
            .setSaleQuantityLCD(quantity.map(
                    q -> Formatters.formatSaleQuantity(q)));
    }
}

