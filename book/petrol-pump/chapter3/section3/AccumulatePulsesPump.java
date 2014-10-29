package chapter3.section3;

import pump.*;
import chapter3.section2.LifeCycle;
import sodium.*;
import java.util.Optional;

public class AccumulatePulsesPump implements Pump {
    public Outputs create(Inputs inputs) {
        LifeCycle lc = new LifeCycle(inputs.eNozzle1,
                                     inputs.eNozzle2,
                                     inputs.eNozzle3);
        Behavior<Double> litersDelivered =
                accumulate(lc.eStart.map(u -> Unit.UNIT),
                           inputs.eFuelPulses,
                           inputs.calibration);
        return new Outputs()
            .setDelivery(lc.fillActive.map(
                of ->
                    of.equals(Optional.of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.equals(Optional.of(Fuel.TWO))   ? Delivery.FAST2 :
                    of.equals(Optional.of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF))
            .setSaleQuantityLCD(litersDelivered.map(
                    q -> Formatters.formatSaleQuantity(q)));
    }

    public static Behavior<Double> accumulate(
            Event<Unit> eClearAccumulator,
            Event<Integer> ePulses,
            Behavior<Double> calibration) {
        BehaviorLoop<Integer> total = new BehaviorLoop<>();
        total.loop(ePulses.snapshot(total,
            (pulses_, total_) -> pulses_ + total_)
               .merge(eClearAccumulator.map(f -> 0))
               .hold(0));
        return Behavior.lift(
            (total_, calibration_) -> total_ * calibration_,
            total, calibration);
    }
}

