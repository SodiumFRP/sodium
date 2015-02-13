import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import sodium.*;

public class HomoZombicus {
    public HomoZombicus(
        World world,
        int self,
        double tInit,
        Point posInit,
        Cell<Double> clock,
        Stream<Unit> sTick,
        Cell<List<Character>> others)
    {
        CellLoop<State> state = new CellLoop<>();
        List<Character> initOthers = new ArrayList<Character>(0);
        Cell<All> all = Cell.lift((st, t, o) -> new All(st, t, o),
            state, clock, others);
        Stream<State> sChange = Stream.filterOptional(
            sTick.snapshot(all,
                (u, a) ->
                    a.t - a.state.t0 >= 0.2
                      ? Optional.of(new State(a.t, a.state.positionAt(a.t),
                          self, a.others))
                      : Optional.<State>empty()
            )
        );
        state.loop(
            sChange.hold(new State(tInit, posInit, self, initOthers))
        );
        character = all.map(a -> new Character(self,
            CharacterType.ZOMBICUS,
            a.state.positionAt(a.t), a.state.velocity));
        sBite = Stream.filterOptional(
            sTick.snapshot(all,
                (u, a) -> {
                    Optional<Character> oVictim = a.state.nearest(self,
                        a.others, false);
                    if (oVictim.isPresent()) {
                        Character victim = oVictim.get();
                        Point myPos = a.state.positionAt(a.t);
                        if (Vector.distance(victim.pos, myPos) < 10)
                            return Optional.<Integer>of(victim.id);
                    }
                    return Optional.<Integer>empty();
                }
            ));
    }
    public final Cell<Character> character;
    public final Stream<Integer> sBite;

    public static final double speed = 20.0;
    private static class State {
        final double t0;
        final Point orig;
        final Vector velocity;

        State(double t0, Point orig, int self, List<Character> others) {
            this.t0 = t0;
            this.orig = orig;
            double bestDist = 0.0;
            Optional<Character> oOther = nearest(self, others, true);
            if (oOther.isPresent()) {
                Character other = oOther.get();
                this.velocity = Vector.subtract(other.pos, orig)
                                      .normalize().mult(
                    other.type == CharacterType.SAPIENS
                                       ? speed : -speed
                );
            }
            else
                this.velocity = new Vector(0,0);
        }

        Optional<Character> nearest(int self, List<Character> others,
                                     boolean includeNearZombies) {
            double bestDist = 0.0;
            Optional<Character> best = Optional.empty();
            for (Character ch : others)
                if (ch.id != self &&
                        (includeNearZombies || ch.type == CharacterType.SAPIENS)) {
                    double dist = Vector.distance(ch.pos, orig);
                    if (ch.type == CharacterType.ZOMBICUS && dist > 60)
                        ;
                    else
                    if (!best.isPresent() || dist < bestDist) {
                        bestDist = dist;
                        best = Optional.of(ch);
                    }
                }
            return best;
        }

        Point positionAt(double t) {
            return velocity.mult(t - t0).add(orig);
        }
    };

    public static class All {
        All(State state, double t, List<Character> others) {
            this.state = state;
            this.t = t;
            this.others = others;
        }
        State state;
        double t;
        List<Character> others;
    };
}

