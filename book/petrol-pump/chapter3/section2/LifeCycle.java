package chapter3.section2;

import pump.*;
import sodium.*;
import java.util.Optional;

public class LifeCycle {
    public Event<Fuel> eStart;
    public Behavior<Optional<Fuel>> fillActive;
    public Event<End> eEnd;

    public enum End { END }

    private static Event<Fuel> whenLifted(Event<UpDown> eNozzle,
                                          Fuel nozzleFuel) {
        return eNozzle.filter(u -> u == UpDown.UP)
                      .map(u -> nozzleFuel);
    }

    private static Event<End> whenSetDown(Event<UpDown> eNozzle,
                Fuel nozzleFuel,
                Behavior<Optional<Fuel>> fillActive) {
        return Event.<End>filterOptional(
            eNozzle.snapshot(fillActive,
                (u,f) -> u == UpDown.DOWN &&
                         f.equals(Optional.of(nozzleFuel))
                                       ? Optional.of(End.END)
                                       : Optional.empty()));
    }

    public LifeCycle(Event<UpDown> eNozzle1,
                     Event<UpDown> eNozzle2,
                     Event<UpDown> eNozzle3) {
        Event<Fuel> eLiftNozzle = whenLifted(eNozzle1, Fuel.ONE).merge(
                                  whenLifted(eNozzle2, Fuel.TWO).merge(
                                  whenLifted(eNozzle3, Fuel.THREE)));
        BehaviorLoop<Optional<Fuel>> fillActive = new BehaviorLoop<>();
        this.fillActive = fillActive;
        this.eStart = Event.filterOptional(
            eLiftNozzle.snapshot(fillActive, (newFuel, fillActive_) ->
                fillActive_.isPresent() ? Optional.empty()
                                        : Optional.of(newFuel)));
        this.eEnd = whenSetDown(eNozzle1, Fuel.ONE, fillActive).merge(
                    whenSetDown(eNozzle2, Fuel.TWO, fillActive).merge(
                    whenSetDown(eNozzle3, Fuel.THREE, fillActive)));
        fillActive.loop(
            eStart.map(f -> Optional.of(f))
                  .merge(eEnd.map(e -> Optional.empty()))
                  .hold(Optional.empty())
        );
    }
}

