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
    static <A> Stream<A> merges(Collection<Stream<A>> in) {
        Stream<A> sOut = new Stream<>();
        for (Stream<A> c : in)
            sOut = sOut.merge(c);
        return sOut;
    }
    public static Stream<Unit> periodicTimer(double t0,
            Cell<Double> time, Stream<Unit> sTick, double period) {
        CellLoop<Double> tAlarm = new CellLoop<>();
        Stream<Double> sAlarm = Stream.filterOptional(
            sTick.snapshot(time).snapshot(tAlarm,
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

        State add(Cell<Character> chr, Stream<Integer> sBite,
                  Stream<Integer> sDestroy) {
            Map<Integer, Cell<Character>> chars =
                                         new HashMap<>(this.chars);
            Map<Integer, Stream<Integer>> sBites =
                                         new HashMap<>(this.sBites);
            Map<Integer, Stream<Integer>> sDestroys =
                                         new HashMap<>(this.sDestroys);
            chars.put(nextID, chr);
            sBites.put(nextID, sBite);
            sDestroys.put(nextID, sDestroy);
            return new State(nextID+1, chars, sBites, sDestroys);
        }
        State remove(int id) {
            Map<Integer, Cell<Character>> chars =
                                         new HashMap<>(this.chars);
            Map<Integer, Stream<Integer>> sBites =
                                         new HashMap<>(this.sBites);
            Map<Integer, Stream<Integer>> sDestroys =
                                         new HashMap<>(this.sDestroys);
            chars.remove(id);
            sBites.remove(id);
            sDestroys.remove(id);
            return new State(nextID, chars, sBites, sDestroys);
        }
    }
    static Stream<Integer> fallDownHole(int self, Stream<Unit> sTick,
                               Cell<Character> character, World world) {
        return Stream.filterOptional(
            sTick.snapshot(character, (u, ch) ->
                world.hitsHole(ch.pos) ? Optional.of(self)
                                       : Optional.<Integer>empty()
            ));
    }
    static class CreateCharacters {
        CreateCharacters(double t0, Cell<Double> time,
                    Stream<Unit> sTick, World world,
                    Cell<List<Character>> scene, Stream<Integer> sBite,
                    Stream<Integer> sDestroy) {
            State initState = new State();
            HomoZombicus z = new HomoZombicus(initState.nextID, t0,
                new Point(36,332), time, sTick, scene);
            initState = initState.add(z.character, z.sBite,
                fallDownHole(initState.nextID, sTick, z.character, world));
            CellLoop<State> state = new CellLoop<>();
            Point center = new Point(world.windowSize.width / 2,
                                     world.windowSize.height / 2);
            Stream<Lambda1<State, State>> sAdd =
                periodicTimer(t0 + 1, time, sTick, 6.0)
                .snapshot(time, (u, t) ->
                    st -> {
                        BitableHomoSapiens h = new BitableHomoSapiens(
                            world, st.nextID, t, center, time, sTick,
                            sBite, scene);
                        return st.add(h.character, h.sBite,
                            fallDownHole(st.nextID, sTick, h.character,
                                world));
                    }
                );
            Stream<Lambda1<State, State>> sRemove = sDestroy.map(id ->
                                                    st -> st.remove(id));
            Stream<Lambda1<State, State>> sChange = sAdd.merge(sRemove,
                (f1, f2) -> a -> f1.apply(f2.apply(a))); 
            state.loop(sChange.snapshot(state, (f, st) -> f.apply(st))
                              .hold(initState));
            Cell<Cell<List<Character>>> cchars = state.map(st ->
                                        sequence(st.chars.values()));
            this.scene = Cell.switchC(cchars);
            Cell<Stream<Integer>> csBite = state.map(st ->
                                        merges(st.sBites.values()));
            this.sBite = Cell.switchS(csBite);
            Cell<Stream<Integer>> csDestroy = state.map(st ->
                                        merges(st.sDestroys.values()));
            this.sDestroy = Cell.switchS(csDestroy);
        }
        final Cell<List<Character>> scene;
        final Stream<Integer> sBite;
        final Stream<Integer> sDestroy;
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
        obstacles.add(new Polygon(
            new int[] { 387, 371, 414, 503, 438, 412 },
            new int[] { 200, 256, 308, 287, 215, 181 },
            6));
        obstacles.add(new Polygon(
            new int[] { 558, 536, 612, 603 },
            new int[] { 124, 191, 228, 155 },
            4));
        Animate.animate(
            "Zombicus dynamic",
            (double t0, Cell<Double> time, Stream<Unit> sTick,
                                            Dimension windowSize) -> {
                World world = new World(windowSize, obstacles);
                CellLoop<List<Character>> scene = new CellLoop<>();
                StreamLoop<Integer> sBite = new StreamLoop<>();
                StreamLoop<Integer> sDestroy = new StreamLoop<>();
                CreateCharacters cc = new CreateCharacters(t0,
                    time, sTick, world, scene, sBite, sDestroy);
                scene.loop(cc.scene);
                sBite.loop(cc.sBite);
                sDestroy.loop(cc.sDestroy);
                return scene;
            },
            obstacles
        );
    }
}

