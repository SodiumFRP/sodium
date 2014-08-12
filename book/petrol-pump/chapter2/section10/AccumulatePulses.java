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

        Behavior<Double> litersDelivered =
                accumulate(inputs.eFuelPulses, inputs.calibration);

        return new Outputs()
            .setDelivery(anyNozzle.map(
                    u -> u == UpDown.UP ? Delivery.FAST1
                                        : Delivery.OFF))
            .setSaleQuantityLCD(litersDelivered.map(
                    q -> Formatters.formatSaleQuantity(q)));
    }

    private static Behavior<Double> accumulate(
            Event<Integer> ePulses, Behavior<Double> calibration)
    {
        BehaviorLoop<Integer> total = new BehaviorLoop<>();
        total.loop(ePulses.snapshot(total,
            (pulses_, total_) -> pulses_ + total_).hold(0));

        return Behavior.lift(
            (total_, calibration_) -> total_ * calibration_,
            total, calibration);
    }
}

