import java.awt.Dimension;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import sodium.*;

public class characters {
    static <A> Cell<List<A>> sequence(Collection<Cell<A>> in) {
        Cell<List<A>> out = new Cell<>(new ArrayList<A>());
        for (Cell<A> c : in)
            out = Cell.lift(
                (l0, a) -> {
                    List<A> l = new ArrayList<A>(l0);
                    l.add(a);
                    return l;
                }, out, c);
        return out;
    }
    static Cell<List<Character>> createCharacters(double t0,
            Cell<Double> time, Stream<Unit> sTick, World world,
            Cell<List<Character>> scene) {
        List<Cell<Character>> chars = new ArrayList<>();
        int id = 0;
        for (int x = 100; x < world.windowSize.width; x += 100)
            for (int y = 150; y < world.windowSize.height; y += 150) {
                Point pos0 = new Point(x, y);
                if (id != 3 && id != 6 && id != 7) {
                    HomoSapiens h = new HomoSapiens(world, id, t0, pos0,
                        time, sTick);
                    chars.add(h.character);
                }
                else {
                    HomoZombicus z = new HomoZombicus(id, t0, pos0,
                        time, sTick, scene);
                    chars.add(z.character);
                }
                id++;
            }
        return sequence(chars);
    }
    public static void main(String[] args)
    {
        Animate.animate(
            "Zombicus characters",
            (double t0, Cell<Double> time, Stream<Unit> sTick,
                                            Dimension windowSize) -> {
                World world = new World(windowSize);
                CellLoop<List<Character>> scene = new CellLoop<>();
                Cell<List<Character>> scene_ = createCharacters(t0,
                    time, sTick, world, scene);
                scene.loop(scene_);
                return scene;
            }
        );
    }
}

