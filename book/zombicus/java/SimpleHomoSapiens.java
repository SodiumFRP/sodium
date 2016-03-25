import java.awt.Point;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import nz.sodium.*;

public class SimpleHomoSapiens {
    public SimpleHomoSapiens(
        int self,
        Point posInit,
        Cell<Double> time,
        Stream<Unit> sTick)
    {
        final double speed = 80.0;
        class Trajectory {
            Trajectory(Random rng, double t0, Point orig) {
                this.t0 = t0;
                this.orig = orig;
                this.period = rng.nextDouble() * 1 + 0.5;
                double angle = rng.nextDouble() * Math.PI * 2;
                velocity = new Vector(Math.sin(angle), Math.cos(angle))
                            .mult(speed);
            }
            double t0;
            Point orig;
            double period;
            Vector velocity;
            Point positionAt(double t) {
                return velocity.mult(t - t0).add(orig);
            }
        }
        Random rng = new Random();
        CellLoop<Trajectory> traj = new CellLoop<>();
        Stream<Unit> sChange = Stream.filterOptional(
            sTick.snapshot(traj, (u, traj_) ->
                time.sample() - traj_.t0 >= traj_.period
                    ? Optional.of(Unit.UNIT)
                    : Optional.<Unit>empty()
            ));
        traj.loop(
            sChange.snapshot(traj, (u, traj_) ->
                new Trajectory(rng, time.sample(),
                    traj_.positionAt(time.sample()))
            ).hold(new Trajectory(rng, time.sample(), posInit))
        );
        character = traj.lift(time, (traj_, t) ->
            new Character(self, CharacterType.SAPIENS,
                traj_.positionAt(t), traj_.velocity)
        );
    }

    public final Cell<Character> character;
}

