import java.awt.Point;
import java.util.List;
import sodium.*;

public class BitableHomoSapiens {
    public BitableHomoSapiens(
        World world,
        int self,
        double tInit,
        Point posInit,
        Cell<Double> time,
        Stream<Unit> sTick,
        Stream<Integer> sBite,
        Cell<List<Character>> scene)
    {
        class All {
            All(Character character, double t) {
                this.character = character;
                this.t = t;
            }
            Character character;
            double t;
        }
    
        HomoSapiens h = new HomoSapiens(world, self, tInit, posInit,
            time, sTick);
        Stream<Integer> sBiteMe = sBite.filter(id -> id == self);
        Cell<All> all = Cell.lift(
            (ch, t) -> new All(ch, t),
            h.character, time);
        Stream<HomoZombicus> sBecome = sBiteMe.snapshot(
            all,
            (id, a) -> new HomoZombicus(
                    self,
                    a.t, a.character.pos,
                    time,
                    sTick, scene 
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

