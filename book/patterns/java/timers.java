import sodium.*;
import sodium.time.*;
import java.util.Optional;

public class timers {
    static Stream<Unit> periodic(TimerSystem sys, long period) {
        Cell<Long> time = sys.clock();
        CellLoop<Optional<Long>> oAlarm = new CellLoop<>();
        Stream<Unit> sAlarm = sys.at(oAlarm);
        oAlarm.loop(
            sAlarm.snapshot(time, (u, t) -> Optional.of(t + period))
                  .hold(Optional.<Long>of(time.sample() + period)));
        return sAlarm;
    }

    public static void main(String[] args) {
        TimerSystem sys = new SimpleTimerSystem();
        Listener l = Transaction.run(() -> {
            Listener l1 = periodic(sys, 1000)
                .listen(u -> { System.out.println("slow"); });
            Listener l2 = periodic(sys, 250)
                .listen(u -> { System.out.println("fast"); });
            return l1.append(l2);
        });
        try { Thread.sleep(2500); } catch (InterruptedException e) {}
        l.unlisten();
    }
}

