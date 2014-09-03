package chapter3.section2;

import pump.*;
import sodium.*;
import java.util.Optional;

public class LifeCycle implements Pump
{
    public enum End { END }

    private static Event<Fuel> whenLifted(
            Event<UpDown> eNozzle, Fuel nozzleFuel)
    {
        return eNozzle.filter(u -> u == UpDown.UP)
                      .map(u -> nozzleFuel);
    }

    private static Event<End> whenSetDown(
            Event<UpDown> eNozzle, Fuel nozzleFuel,
            Behavior<Optional<Fuel>> fillActive)
    {
        return Event.<End>filterOptional(
            eNozzle.snapshot(fillActive,
                (u,f) -> u == UpDown.DOWN &&
                         f.equals(Optional.of(nozzleFuel))
                                       ? Optional.of(End.END)
                                       : Optional.empty()));
    }

    public static class LifeCycleOut {
        public LifeCycleOut(Event<Fuel> eStart,
                         Behavior<Optional<Fuel>> fillActive,
                         Event<End> eEnd)
        {
            this.eStart = eStart;
            this.fillActive = fillActive;
            this.eEnd = eEnd;
        }
        public Event<Fuel> eStart;
        public Behavior<Optional<Fuel>> fillActive;
        public Event<End> eEnd;
    };

    public static LifeCycleOut lifeCycle(
        Event<UpDown> eNozzle1,
        Event<UpDown> eNozzle2,
        Event<UpDown> eNozzle3)
    {
        Event<Fuel> eLiftNozzle = whenLifted(eNozzle1, Fuel.ONE).merge(
                                  whenLifted(eNozzle2, Fuel.TWO).merge(
                                  whenLifted(eNozzle3, Fuel.THREE)));

        BehaviorLoop<Optional<Fuel>> fillActive = new BehaviorLoop<>();

        Event<Fuel> eStart = Event.filterOptional(
            eLiftNozzle.snapshot(fillActive, (newFuel, fillActive_) ->
                fillActive_.isPresent() ? Optional.empty()
                                          : Optional.of(newFuel)));

        Event<End> eEnd =
                whenSetDown(eNozzle1, Fuel.ONE, fillActive).merge(
                whenSetDown(eNozzle2, Fuel.TWO, fillActive).merge(
                whenSetDown(eNozzle3, Fuel.THREE, fillActive)));

        fillActive.loop(
            eStart.map(f -> Optional.of(f)).merge(
                eEnd.map(e -> Optional.empty()))
            .hold(Optional.empty()));
        return new LifeCycleOut(eStart, fillActive, eEnd);
    }

    public Outputs create(Inputs inputs)
    {
        LifeCycleOut lc = lifeCycle(inputs.eNozzle1,
                                    inputs.eNozzle2,
                                    inputs.eNozzle3);
        return new Outputs()
            .setDelivery(lc.fillActive.map(
                of ->
                    of.equals(Optional.of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.equals(Optional.of(Fuel.TWO))   ? Delivery.FAST2 :
                    of.equals(Optional.of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF))
            .setSaleQuantityLCD(lc.fillActive.map(
                of ->
                    of.equals(Optional.of(Fuel.ONE))   ? "1" :
                    of.equals(Optional.of(Fuel.TWO))   ? "2" :
                    of.equals(Optional.of(Fuel.THREE)) ? "3" : ""));
    }
}

