import java.awt.Point;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import sodium.*;

public class BitableHomoSapien {
    private static class All {
        All(Character character, double t) {
            this.character = character;
            this.t = t;
        }
        Character character;
        double t;
    }
    public BitableHomoSapien(
        World world,
        int self,
        double tInit,
        Point posInit,
        Cell<Double> clock,
        Stream<Unit> sTick,
        Stream<Integer> sBite,
        Cell<List<Character>> others)
    {
        HomoSapien h = new HomoSapien(world, self, tInit, posInit, clock, sTick);
        Stream<Integer> sBiteMe = sBite.filter(id -> id == self);
        Cell<All> all = Cell.lift(
            (ch, t) -> new All(ch, t),
            h.character, clock);
        Stream<HomoZombicus> sBecome = sBiteMe.snapshot(
            all,
            (id, a) -> new HomoZombicus(
                    world, self,
                    a.t, a.character.pos,
                    clock,
                    sTick, others
                )
        );
        this.character = Cell.switchC(
            sBecome.map(z -> z.character).hold(h.character)
        );
        this.sBite = Cell.switchS(
            sBecome.map(z -> z.sBite).hold(new Stream<Integer>())
        );
    }
    public final Cell<Character> character;
    public final Stream<Integer> sBite;
}

