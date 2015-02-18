import java.awt.Dimension;
import java.awt.Point;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import sodium.*;

public class dynamic {
    public static <A> Cell<List<A>> sequence(Collection<Cell<A>> in)
    {
        Cell<List<A>> out = new Cell<>(new ArrayList<A>());
        for (Cell<A> c : in)
            out = Cell.lift(
                (l0, a) -> {
                    List<A> l = new ArrayList<A>(l0);
                    l.add(a);
                    return l;
                },
                out, c);
        return out;
    }

    public static <A> Stream<A> merges(Collection<Stream<A>> in)
    {
        Stream<A> sOut = new Stream<>();
        for (Stream<A> c : in)
            sOut = sOut.merge(c);
        return sOut;
    }

    public static Stream<Unit> periodicTimer(double t0, Cell<Double> clock, Stream<Unit> sTick, double period)
    {
        CellLoop<Double> tAlarm = new CellLoop<>();
        Stream<Double> sAlarm =
            Stream.filterOptional(
                sTick.snapshot(clock)
                     .snapshot(tAlarm,
                        (t, alarm) -> t >= alarm ? Optional.of(t + period)
                                                 : Optional.<Double>empty())
            );
        tAlarm.loop(sAlarm.hold(t0));
        return sAlarm.map(u -> Unit.UNIT);
    }

    static class State {
        State() {
            this.nextID = 0;
            this.chars = new HashMap<>();
            this.sBites = new HashMap<>();
            this.sDestroys = new HashMap<>();
        }
        State(int nextID, Map<Integer, Cell<Character>> chars,
                          Map<Integer, Stream<Integer>> sBites,
                          Map<Integer, Stream<Integer>> sDestroys) {
            this.nextID = nextID;
            this.chars = chars;
            this.sBites = sBites;
            this.sDestroys = sDestroys;
        }
        final int nextID;
        final Map<Integer, Cell<Character>> chars;
        final Map<Integer, Stream<Integer>> sBites;
        final Map<Integer, Stream<Integer>> sDestroys;

        State add(Cell<Character> chr, Stream<Integer> sBite, Stream<Integer> sDestroy) {
            Map<Integer, Cell<Character>> chars = new HashMap<>(this.chars);
            chars.put(nextID, chr);
            Map<Integer, Stream<Integer>> sBites = new HashMap<>(this.sBites);
            sBites.put(nextID, sBite);
            Map<Integer, Stream<Integer>> sDestroys = new HashMap<>(this.sDestroys);
            sDestroys.put(nextID, sDestroy);
            return new State(nextID+1, chars, sBites, sDestroys);
        }
        State remove(int id) {
            Map<Integer, Cell<Character>> chars = new HashMap<>(this.chars);
            chars.remove(id);
            Map<Integer, Stream<Integer>> sBites = new HashMap<>(this.sBites);
            sBites.remove(id);
            Map<Integer, Stream<Integer>> sDestroys = new HashMap<>(this.sDestroys);
            sDestroys.remove(id);
            return new State(nextID, chars, sBites, sDestroys);
        }
    }

    static Stream<Integer> fallDownHole()
    {
        return new Stream<Integer>();
    }

    public static void main(String[] args)
    {
        ArrayList<Polygon> obstacles = new ArrayList<>();
        obstacles.add(new Polygon(
            new int[] { 116, 134, 190, 248, 337, 245, 185 },
            new int[] { 208, 129, 121, 79, 128, 172, 231 },
            7));
        obstacles.add(new Polygon(
            new int[] { 203, 250, 342, 455, 515, 467, 286 },
            new int[] { 376, 337, 369, 350, 401, 438, 425 },
            7));

        Animate.animate(
            "Zombicus dynamic",
(double t0, Cell<Double> clock, Stream<Unit> sTick,
                                Dimension windowSize) -> {
    World world = new World(windowSize, obstacles);
    CellLoop<List<Character>> scene = new CellLoop<>();

    State initState = new State();
    HomoZombicus z = new HomoZombicus(initState.nextID, t0, new Point(596,156),
        clock, sTick, scene);
    initState = initState.add(z.character, z.sBite, fallDownHole());

    Point center = new Point(windowSize.width / 2, windowSize.height / 2);

    Stream<Unit> sAlarm = periodicTimer(t0 + 1, clock, sTick, 4.0);
    CellLoop<State> state = new CellLoop<>();
    StreamLoop<Integer> sBite = new StreamLoop<>();
    Stream<Lambda1<State, State>> sCreate =
        sAlarm.snapshot(clock,
                (u, t) ->
                    st -> {
                        BitableHomoSapiens h = new BitableHomoSapiens(world, st.nextID,
                                t, center, clock, sTick, sBite, scene);
                        return st.add(h.character, h.sBite, fallDownHole());
                    }
            );
    Stream<Lambda1<State, State>> sDestroy = new Stream<>();
    Stream<Lambda1<State, State>> sChange = sCreate
            .merge(sDestroy, (f1, f2) -> a -> f1.apply(f2.apply(a))); 
    state.loop(
        sChange
            .snapshot(state, (f, st) -> f.apply(st))
            .hold(initState)
    );
    Cell<Cell<List<Character>>> cchars = state.map(st -> sequence(st.chars.values()));
    Cell<List<Character>> chars = Cell.switchC(cchars);
    ArrayList<Character> emptyScene = new ArrayList<>();
    scene.loop(chars.updates().hold(emptyScene));

    Cell<Stream<Integer>> csBites = state.map(st -> merges(st.sBites.values()));
    sBite.loop(Cell.switchS(csBites));

    return scene;
},
obstacles

        );
    }
}

