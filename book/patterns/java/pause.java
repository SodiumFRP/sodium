import sodium.*;
import java.util.Optional;

public class pause {
    public static <A> Stream<Unit> changeTo(Cell<A> a, A target) {
        return Stream.filterOptional(
            Operational.updates(a).snapshot(a,
                (neu, old) -> neu.equals(target) && !neu.equals(old)
                    ? Optional.of(Unit.UNIT)
                    : Optional.empty()
            ));
    }

    public static Cell<Double> pausableClock(Stream<Unit> sPause,
            Stream<Unit> sResume, Cell<Double> clock) {
        Cell<Optional<Double>> pauseTime =
            sPause.snapshot(clock, (u, t) -> Optional.<Double>of(t))
                .merge(sResume.map(u -> Optional.<Double>empty()))
                .hold(Optional.<Double>empty());
        Cell<Double> lostTime = sResume.<Double>accum(
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
        StreamSink<Unit> sPause = new StreamSink<>();
        StreamSink<Unit> sResume = new StreamSink<>();
        Cell<Double> gameClock = pausableClock(sPause, sResume, mainClock);
        Listener l = Cell.lift((m, g) -> "main="+m+" game="+g,
                               mainClock, gameClock)
                         .listen(txt -> System.out.println(txt));
        mainClock.send(1.0);
        mainClock.send(2.0);
        mainClock.send(3.0);
        sPause.send(Unit.UNIT);
        mainClock.send(4.0);
        mainClock.send(5.0);
        mainClock.send(6.0);
        sResume.send(Unit.UNIT);
        mainClock.send(7.0);
        l.unlisten();
    }
}

