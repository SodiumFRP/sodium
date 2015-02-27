import java.awt.Dimension;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import sodium.*;

public class humans {
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
    public static void main(String[] args)
    {
        Animate.animate(
            "Zombicus humans",
            (double t0, Cell<Double> time, Stream<Unit> sTick,
                                            Dimension windowSize) -> {
                World world = new World(windowSize);
                List<Cell<Character>> chars = new ArrayList<>();
                int id = 0;
                for (int x = 100; x < windowSize.width; x += 100)
                    for (int y = 150; y < windowSize.height; y += 150) {
                        Point pos0 = new Point(x, y);
                        HomoSapiens h = new HomoSapiens(world, id,
                            t0, pos0, time, sTick);
                        chars.add(h.character);
                        id++;
                    }
                return sequence(chars);
            }
        );
    }
}

