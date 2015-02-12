import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import sodium.*;

public class HomoZombicus {
    public static final double velocity = 80.0;

    public static class State {
        State(double t0, Point orig, int self, List<Character> others) {
            this.t0 = t0;
            this.orig = orig;
            double bestDist = 0.0;
            Vector bestVec = null;
            for (Character o : others)
                if (o.id != self) {
                    Vector vec = Vector.subtract(o.pos, orig);
                    double dist = vec.magnitude();
                    if (bestVec == null || dist < bestDist) {
                        bestDist = dist;
                        bestVec = vec;
                    }
                }
            this.velocity = bestVec == null ? new Vector(0,0)
                                            : bestVec.normalize();
        }
        double t0;
        Point orig;
        Vector velocity;
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

    public static Cell<Character> create(
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
        state.loop(
            sTick.snapshot(all,
                (u, a) -> new State(a.t, a.state.positionAt(a.t), self, a.others)
            ).hold(new State(tInit, posInit, self, initOthers))
        );
        return all.map(a -> new Character(self, CharacterType.ZOMBICUS,
            a.state.positionAt(a.t),
            a.state.velocity));
    }
}

