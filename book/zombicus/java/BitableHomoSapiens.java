import java.awt.Point;
import java.util.List;
import sodium.*;

public class BitableHomoSapiens {
    public BitableHomoSapiens(
        World world,
        int self,
        Point posInit,
        Cell<Double> time,
        Stream<Unit> sTick,
        Stream<Integer> sBite,
        Cell<List<Character>> scene)
    {
        HomoSapiens h = new HomoSapiens(world, self, posInit,
            time, sTick);
        Stream<Integer> sBiteMe = sBite.filter(id -> id == self);
        Stream<HomoZombicus> sBecome = sBiteMe.snapshot(
            h.character,
            (id, ch) -> new HomoZombicus(
                    self,
                    ch.pos,
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

