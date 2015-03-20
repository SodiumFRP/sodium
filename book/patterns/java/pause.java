import sodium.*;
import java.util.Optional;

public class pause {
    public static <A> Stream<Unit> changeTo(Cell<A> a, A target) {
        return Stream.filterOptional(
            a.updates().snapshot(a,
                (neu, old) -> neu.equals(target) && !neu.equals(old)
                    ? Optional.of(Unit.UNIT)
                    : Optional.empty()
            ));
    }

    public static Cell<Double> pausableClock(Cell<Boolean> paused,
            Cell<Double> clock) {
        Stream<Unit> sPause = changeTo(paused, true);
        Stream<Unit> sUnpause = changeTo(paused, false);
        Cell<Optional<Double>> pauseTime =
            sPause.snapshot(clock, (u, t) -> Optional.<Double>of(t))
                .merge(sUnpause.map(u -> Optional.<Double>empty()))
                .hold(Optional.<Double>empty());
        Cell<Double> lostTime = sUnpause.<Double>accum(
            0.0,
            (u, total) -> {
                double tPause = pauseTime.sample().get();
                double now    = clock.sample();
                return total + (now - tPause);
            });
        return Cell.lift((otPause, tClk, tLost) ->
            (otPause.isPresent() ? otPause.get()
                                 : tClk)
            - tLost,
            pauseTime, clock, lostTime);
    }

    public static void main(String[] args) {
        CellSink<Double> mainClock = new CellSink<>(0.0);
        CellSink<Boolean> paused = new CellSink<>(false);
        Cell<Double> gameClock = pausableClock(paused, mainClock);
        Listener l = Cell.lift((m, g) -> "main="+m+" game="+g,
                               mainClock, gameClock)
                         .listen(txt -> System.out.println(txt));
        mainClock.send(1.0);
        mainClock.send(2.0);
        mainClock.send(3.0);
        paused.send(true);
        mainClock.send(4.0);
        mainClock.send(5.0);
        mainClock.send(6.0);
        paused.send(false);
        mainClock.send(7.0);
        l.unlisten();
    }
}

