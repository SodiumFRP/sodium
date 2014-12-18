package chapter2.section13;

import pump.*;
import sodium.*;

public class AccumulatePulses implements Pump
{
    public Outputs create(Inputs inputs) {
        Cell<UpDown> nozzle1 = inputs.sNozzle1.hold(UpDown.DOWN);
        Cell<UpDown> nozzle2 = inputs.sNozzle2.hold(UpDown.DOWN);
        Cell<UpDown> nozzle3 = inputs.sNozzle3.hold(UpDown.DOWN);
        Cell<UpDown> anyNozzle = Cell.lift((n1, n2, n3) ->
            n1 == UpDown.UP || n2 == UpDown.UP || n3 == UpDown.UP
                    ? UpDown.UP : UpDown.DOWN,
            nozzle1, nozzle2, nozzle3);
        Cell<Double> litersDelivered =
                accumulate(inputs.sFuelPulses, inputs.calibration);
        return new Outputs()
            .setDelivery(anyNozzle.map(
                    u -> u == UpDown.UP ? Delivery.FAST1
                                        : Delivery.OFF))
            .setSaleQuantityLCD(litersDelivered.map(
                    q -> Formatters.formatSaleQuantity(q)));
    }

    private static Cell<Double> accumulate(
            Stream<Integer> sPulses, Cell<Double> calibration) {
        CellLoop<Integer> total = new CellLoop<>();
        total.loop(sPulses.snapshot(total,
            (pulses_, total_) -> pulses_ + total_).hold(0));
        return Cell.lift(
            (total_, calibration_) -> total_ * calibration_,
            total, calibration);
    }
}

