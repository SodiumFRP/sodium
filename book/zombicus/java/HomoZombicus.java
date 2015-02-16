import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import sodium.*;

public class HomoZombicus {
    public HomoZombicus(
        int self,
        double tInit,
        Point posInit,
        Cell<Double> clock,
        Stream<Unit> sTick,
        Cell<List<Character>> scene)
    {
        final double speed = 20.0;

        class State {
            State(double t0, Point orig, int self,
                                               List<Character> scene) {
                this.t0 = t0;
                this.orig = orig;
                double bestDist = 0.0;
                Optional<Character> oOther = nearest(self, scene, true);
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
    
            Optional<Character> nearest(int self, List<Character> scene,
                                         boolean includeNearZombies) {
                double bestDist = 0.0;
                Optional<Character> best = Optional.empty();
                for (Character ch : scene)
                    if (ch.id != self && (includeNearZombies
                                    || ch.type == CharacterType.SAPIENS)) {
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
            final double t0;
            final Point orig;
            final Vector velocity;
            Point positionAt(double t) {
                return velocity.mult(t - t0).add(orig);
            }
        }

        class All {
            All(State state, double t, List<Character> scene) {
                this.state = state;
                this.t = t;
                this.scene = scene;
            }
            State state;
            double t;
            List<Character> scene;
        }

        CellLoop<State> state = new CellLoop<>();
        Cell<All> all = Cell.lift((st, t, sc) -> new All(st, t, sc),
            state, clock, scene);
        Stream<State> sChange = Stream.filterOptional(
            sTick.snapshot(all,
                (u, a) ->
                    a.t - a.state.t0 >= 0.2
                      ? Optional.of(new State(a.t, a.state.positionAt(a.t),
                          self, a.scene))
                      : Optional.<State>empty()
            )
        );
        List<Character> emptyScene = new ArrayList<Character>(0);
        state.loop(
            sChange.hold(new State(tInit, posInit, self, emptyScene))
        );
        character = all.map(a -> new Character(self,
            CharacterType.ZOMBICUS,
            a.state.positionAt(a.t), a.state.velocity));
        sBite = Stream.filterOptional(
            sTick.snapshot(all,
                (u, a) -> {
                    Optional<Character> oVictim = a.state.nearest(self,
                        a.scene, false);
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
}

