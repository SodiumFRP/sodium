import java.awt.Point;
import java.util.Optional;
import java.util.Random;
import sodium.*;

public class HomoSapien {
    public static final double speed = 80.0;
    public static final double step = 0.02;

    static class State {
        State(World world, Random rng, double t0, Point orig) {
            this.t0 = t0;
            this.orig = orig;
            this.period = rng.nextDouble() * 1 + 0.5;
            for (int i = 0; i < 10; i++) {
                double angle = rng.nextDouble() * Math.PI * 2;
                velocity = new Vector(Math.sin(angle), Math.cos(angle))
                            .mult(speed);
                if (!world.hitsObstacle(positionAt(t0 + step*2)))
                    break;
            }
        }
        double t0;
        Point orig;
        double period;
        Vector velocity;
        Point positionAt(double t) {
            return velocity.mult(t - t0).add(orig);
        }
    }

    public static Cell<Character> create(
        World world,
        int self,
        double tInit,
        Point posInit,
        Cell<Double> clock,
        Stream<Unit> sTick)
    {
        Random rng = new Random();
        CellLoop<State> state = new CellLoop<>();
        Cell<Tuple2<State, Double>> stateAndClock = Cell.lift(
            (st, clk) -> new Tuple2<State, Double>(st, clk),
            state, clock);
        Stream<Unit> sChange = Stream.filterOptional(
            sTick.snapshot(stateAndClock,
                (u, stclk) -> {
                    State st = stclk.a;
                    double t = stclk.b;
                    if (world.hitsObstacle(st.positionAt(t + step))
                        || t - st.t0 >= st.period)
                        return Optional.of(Unit.UNIT);
                    else
                        return Optional.<Unit>empty();
                }));
        state.loop(
            sChange.snapshot(stateAndClock, (u, stclk) -> {
                State st = stclk.a;
                double t = stclk.b;
                return new State(world, rng, t, st.positionAt(t));
            }).hold(new State(world, rng, tInit, posInit))
        );
        return stateAndClock.map(stclk -> {
                State st = stclk.a;
                double t = stclk.b;
                return new Character(self, CharacterType.SAPIEN,
                    st.positionAt(t), st.velocity);
            });
    }
}

