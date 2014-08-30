package chapter3.section1;

import pump.*;
import sodium.*;
import java.util.Optional;

public class LifeCycle implements Pump
{
    enum Up { UP }
    public enum End { END }
    public enum Fuel { ONE, TWO, THREE }

    private static Event<Fuel> whenLifted(
            Event<UpDown> eNozzle, Fuel nozzleFuel)
    {
        return Event.<Fuel>filterOptional(
            eNozzle.map(
                u -> u == UpDown.UP ? Optional.of(nozzleFuel)
                                    : Optional.empty()));
    }

    private static Event<End> whenSetDown(
            Event<UpDown> eNozzle, Fuel nozzleFuel,
            Behavior<Optional<Fuel>> fuelSelected)
    {
        return Event.<End>filterOptional(
            eNozzle.snapshot(fuelSelected,
                (u,f) -> u == UpDown.DOWN &&
                         f.equals(Optional.of(nozzleFuel))
                                       ? Optional.of(End.END)
                                       : Optional.empty()));
    }

    public static class LifeCycleOut {
        public LifeCycleOut(Event<Fuel> eStart,
                         Behavior<Optional<Fuel>> fuelSelected,
                         Event<End> eEnd)
        {
            this.eStart = eStart;
            this.fuelSelected = fuelSelected;
            this.eEnd = eEnd;
        }
        Event<Fuel> eStart;
        Behavior<Optional<Fuel>> fuelSelected;
        Event<End> eEnd;
    };

    public static LifeCycleOut lifeCycle(
        Event<UpDown> eNozzle1,
        Event<UpDown> eNozzle2,
        Event<UpDown> eNozzle3)
    {
        Event<Fuel> eLiftNozzle = whenLifted(eNozzle1, Fuel.ONE).merge(
                                  whenLifted(eNozzle2, Fuel.TWO).merge(
                                  whenLifted(eNozzle3, Fuel.THREE)));

        BehaviorLoop<Optional<Fuel>> fuelSelected = new BehaviorLoop();

        Event<Fuel> eStart = Event.filterOptional(
            eLiftNozzle.snapshot(fuelSelected, (newFuel, fuelSelected_) ->
                fuelSelected_.isPresent() ? Optional.empty()
                                     : Optional.of(newFuel)));

        Event<End> eEnd = whenSetDown(eNozzle1, Fuel.ONE, fuelSelected).merge(
                          whenSetDown(eNozzle2, Fuel.TWO, fuelSelected).merge(
                          whenSetDown(eNozzle3, Fuel.THREE, fuelSelected)));

        fuelSelected.loop(
            eStart.map(f -> Optional.of(f)).merge(
                eEnd.map(e -> Optional.empty()))
            .hold(Optional.empty()));
        return new LifeCycleOut(eStart, fuelSelected, eEnd);
    }

    public Outputs create(Inputs inputs)
    {
        LifeCycleOut lc = lifeCycle(inputs.eNozzle1,
                                    inputs.eNozzle2,
                                    inputs.eNozzle3);
        return new Outputs()
            .setDelivery(lc.fuelSelected.map(
                of ->
                    of.equals(Optional.of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.equals(Optional.of(Fuel.TWO))   ? Delivery.FAST2 :
                    of.equals(Optional.of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                       Delivery.OFF))
            .setSaleQuantityLCD(lc.fuelSelected.map(
                of ->
                    of.equals(Optional.of(Fuel.ONE))   ? "1" :
                    of.equals(Optional.of(Fuel.TWO))   ? "2" :
                    of.equals(Optional.of(Fuel.THREE)) ? "3" : ""));
    }

    /*
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
    */
}

