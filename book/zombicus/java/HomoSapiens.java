import java.awt.Point;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import sodium.*;

public class HomoSapiens {
    public static final double speed = 80.0;
    public static final double step = 0.02;

    private static class All {
        All(State state, double t) {
            this.state = state;
            this.t = t;
        }
        final State state;
        final double t;
    };

    public HomoSapiens(
        World world,
        int self,
        double tInit,
        Point posInit,
        Cell<Double> clock,
        Stream<Unit> sTick)
    {
        Random rng = new Random();
        CellLoop<State> state = new CellLoop<>();
        Cell<All> all = Cell.lift(
            (st, clk) -> new All(st, clk),
            state, clock);
        Stream<Unit> sChange = Stream.filterOptional(
            sTick.snapshot(all,
                (u, a) -> {
                    if (world.hitsObstacle(a.state.positionAt(a.t + step))
                        || a.t - a.state.t0 >= a.state.period)
                        return Optional.of(Unit.UNIT);
                    else
                        return Optional.<Unit>empty();
                }));
        state.loop(
            sChange.snapshot(all, (u, a) -> {
                return new State(world, rng, a.t, a.state.positionAt(a.t));
            }).hold(new State(world, rng, tInit, posInit))
        );
        character = all.map(a -> {
                return new Character(self, CharacterType.SAPIENS,
                    a.state.positionAt(a.t), a.state.velocity);
            });
    }

    public final Cell<Character> character;

    private static class State {
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
}

