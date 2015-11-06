package chapter4.section4;

import pump.*;
import nz.sodium.*;
import java.util.Optional;

public class LifeCycle {
    public final Stream<Fuel> sStart;
    public final Cell<Optional<Fuel>> fillActive;
    public final Stream<End> sEnd;

    public enum End { END }

    private static Stream<Fuel> whenLifted(Stream<UpDown> sNozzle,
                                          Fuel nozzleFuel) {
        return sNozzle.filter(u -> u == UpDown.UP)
                      .map(u -> nozzleFuel);
    }

    private static Stream<End> whenSetDown(Stream<UpDown> sNozzle,
                Fuel nozzleFuel,
                Cell<Optional<Fuel>> fillActive) {
        return Stream.<End>filterOptional(
            sNozzle.snapshot(fillActive,
                (u,f) -> u == UpDown.DOWN &&
                         f.equals(Optional.of(nozzleFuel))
                                       ? Optional.of(End.END)
                                       : Optional.empty()));
    }

    public LifeCycle(Stream<UpDown> sNozzle1,
                     Stream<UpDown> sNozzle2,
                     Stream<UpDown> sNozzle3) {
        Stream<Fuel> sLiftNozzle =
            whenLifted(sNozzle1, Fuel.ONE).orElse(
            whenLifted(sNozzle2, Fuel.TWO).orElse(
            whenLifted(sNozzle3, Fuel.THREE)));
        CellLoop<Optional<Fuel>> fillActive = new CellLoop<>();
        this.fillActive = fillActive;
        this.sStart = Stream.filterOptional(
            sLiftNozzle.snapshot(fillActive, (newFuel, fillActive_) ->
                fillActive_.isPresent() ? Optional.empty()
                                        : Optional.of(newFuel)));
        this.sEnd = whenSetDown(sNozzle1, Fuel.ONE, fillActive).orElse(
                    whenSetDown(sNozzle2, Fuel.TWO, fillActive).orElse(
                    whenSetDown(sNozzle3, Fuel.THREE, fillActive)));
        fillActive.loop(
            sEnd.map(e -> Optional.<Fuel>empty())
                .orElse(sStart.map(f -> Optional.of(f)))
                .hold(Optional.empty())
        );
    }
}

