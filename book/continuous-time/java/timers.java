import nz.sodium.*;
import nz.sodium.time.*;
import java.util.Optional;

public class timers {
    static Stream<Long> periodic(TimerSystem sys, long period) {
        Cell<Long> time = sys.time;
        CellLoop<Optional<Long>> oAlarm = new CellLoop<>();
        Stream<Long> sAlarm = sys.at(oAlarm);
        oAlarm.loop(
            sAlarm.map(t -> Optional.of(t + period))
                  .hold(Optional.<Long>of(time.sample() + period)));
        return sAlarm;
    }

    public static void main(String[] args) {
        TimerSystem sys = new MillisecondsTimerSystem();
        Cell<Long> time = sys.time;
        StreamSink<Unit> sMain = new StreamSink<Unit>();
        Listener l = Transaction.run(() -> {
            long t0 = time.sample();
            Listener l1 = periodic(sys, 1000).listen(t -> {
                System.out.println((t - t0)+" timer"); });
            Listener l2 = sMain.snapshot(time).listen(t -> {
                System.out.println((t - t0)+" main");
            });
            return l1.append(l2);
        });
        for (int i = 0; i < 5; i++) {
            sMain.send(Unit.UNIT);
            try { Thread.sleep(990); } catch (InterruptedException e) {}
        }
        l.unlisten();
    }
}

