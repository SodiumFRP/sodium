import java.awt.Dimension;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import sodium.*;

public class characters {
    public static void main(String[] args)
    {
        Animate.animate(
            "Zombicus characters",

(double t0, Cell<Double> clock, Stream<Unit> sTick,
                                Dimension windowSize) -> {
    World world = new World(windowSize);
    List<Cell<Character>> chars = new ArrayList<>();
    CellLoop<List<Character>> others = new CellLoop<>();
    int id = 0;
    for (int x = 100; x < 600; x += 100)
        for (int y = 150; y < 400; y += 150) {
            Point pos0 = new Point(x, y);
            if (id != 3 && id != 6 && id != 7) {
                HomoSapien h = new HomoSapien(world, id, t0, pos0,
                    clock, sTick);
                chars.add(h.character);
            }
            else {
                HomoZombicus z = new HomoZombicus(world, id, t0, pos0,
                    clock, sTick, others);
                chars.add(z.character);
            }
            id++;
        }
    Cell<List<Character>> characters = new Cell<>(new ArrayList<Character>());
    for (Cell<Character> c : chars) {
        characters = Cell.lift(
            (cc, l0) -> {
                List<Character> l = new ArrayList<Character>(l0);
                l.add(cc);
                return l;
            },
            c, characters);
    }
    others.loop(characters.updates().hold(new ArrayList<>()));
    return characters;
}

        );
    }
}

