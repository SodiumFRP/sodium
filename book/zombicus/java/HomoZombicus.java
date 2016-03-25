import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import nz.sodium.*;

public class HomoZombicus {
    public HomoZombicus(
        int self,
        Point posInit,
        Cell<Double> time,
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
                Optional<Character> oOther = nearest(self, scene);
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
            Optional<Character> nearest(int self, List<Character> scene) {
                double bestDist = 0.0;
                Optional<Character> best = Optional.empty();
                for (Character ch : scene)
                    if (ch.id != self) {
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
            Optional<Character> nearestSapiens(int self,
                                               List<Character> scene) {
                List<Character> sapiens = new ArrayList<>();
                for (Character ch : scene) {
                    if (ch.type == CharacterType.SAPIENS)
                        sapiens.add(ch);
                }
                return nearest(self, sapiens);
            }
            final double t0;
            final Point orig;
            final Vector velocity;
            Point positionAt(double t) {
                return velocity.mult(t - t0).add(orig);
            }
        }

        CellLoop<State> state = new CellLoop<>();
        Stream<State> sChange = Stream.filterOptional(
            sTick.snapshot(state,
                (u, st) -> {
                    double t = time.sample();
                    return t - st.t0 >= 0.2
                        ? Optional.of(new State(t, st.positionAt(t),
                            self, scene.sample()))
                        : Optional.<State>empty();
                }
            ));
        List<Character> emptyScene = new ArrayList<Character>(0);
        state.loop(sChange.hold(
            new State(time.sample(), posInit, self, emptyScene)
        ));
        character = state.lift(time, (st, t) ->
            new Character(self, CharacterType.ZOMBICUS,
                st.positionAt(time.sample()), st.velocity));
        sBite = Stream.filterOptional(
            sTick.snapshot(state,
                (u, st) -> {
                    Optional<Character> oVictim = st.nearestSapiens(
                        self, scene.sample());
                    if (oVictim.isPresent()) {
                        Character victim = oVictim.get();
                        Point myPos = st.positionAt(time.sample());
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

