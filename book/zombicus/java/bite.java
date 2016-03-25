import java.awt.Dimension;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import nz.sodium.*;

public class bite {
    static <A> Cell<List<A>> sequence(Collection<Cell<A>> in) {
        Cell<List<A>> out = new Cell<>(new ArrayList<A>());
        for (Cell<A> c : in)
            out = out.lift(c,
                (list0, a) -> {
                    List<A> list = new ArrayList<A>(list0);
                    list.add(a);
                    return list;
                });
        return out;
    }
    static class CreateCharacters {
        CreateCharacters(Cell<Double> time,
                Stream<Unit> sTick, World world,
                Cell<List<Character>> scene, Stream<Set<Integer>> sBite) {
            List<Cell<Character>> chars = new ArrayList<>();
            List<Stream<Integer>> sBites = new ArrayList<>();
            int id = 0;
            for (int x = 100; x < world.windowSize.width; x += 80)
                for (int y = 150; y < world.windowSize.height; y += 120) {
                    Point pos0 = new Point(x, y);
                    if (id != 3 && id != 21) {
                        BitableHomoSapiens h = new BitableHomoSapiens(world, id,
                            pos0, time, sTick,
                            sBite, scene);
                        chars.add(h.character);
                        sBites.add(h.sBite);
                    }
                    else {
                        HomoZombicus z = new HomoZombicus(id, pos0,
                            time, sTick, scene);
                        chars.add(z.character);
                        sBites.add(z.sBite);
                    }
                    id++;
                }
            this.scene = sequence(chars);
            this.sBite = Helper.mergeToSet(sBites);
        }
        final Cell<List<Character>> scene;
        final Stream<Set<Integer>> sBite;
    }
    public static void main(String[] args)
    {
        Animate.animate(
            "Zombicus bite",
            
            (Cell<Double> time, Stream<Unit> sTick,
                                            Dimension windowSize) -> {
                World world = new World(windowSize);
                CellLoop<List<Character>> scene = new CellLoop<>();
                StreamLoop<Set<Integer>> sBite = new StreamLoop<>();
                CreateCharacters cc = new CreateCharacters(
                    time, sTick, world, scene, sBite);
                scene.loop(cc.scene);
                sBite.loop(cc.sBite);
                return scene;
            }

        );
    }
}

