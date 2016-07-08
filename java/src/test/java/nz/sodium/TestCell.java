package nz.sodium;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import junit.framework.TestCase;

public class TestCell extends TestCase {
    @Override
    protected void tearDown() throws Exception {
        System.gc();
        Thread.sleep(100);
    }

    public void testHold() {
        StreamSink<Integer> e = new StreamSink<Integer>();
        Cell<Integer> b = e.hold(0);
        List<Integer> out = new ArrayList<Integer>();
        Listener l = Operational.updates(b).listen(x -> {
            out.add(x);
        });
        e.send(2);
        e.send(9);
        l.unlisten();
        assertEquals(Arrays.asList(2, 9), out);
    }

    public void testSnapshot() {
        CellSink<Integer> b = new CellSink<Integer>(0);
        StreamSink<Long> trigger = new StreamSink<Long>();
        List<String> out = new ArrayList<String>();
        Listener l = trigger.snapshot(b, (x, y) -> x + " " + y)
                .listen(x -> {
                    out.add(x);
                });
        trigger.send(100L);
        b.send(2);
        trigger.send(200L);
        b.send(9);
        b.send(1);
        trigger.send(300L);
        l.unlisten();
        assertEquals(Arrays.asList("100 0", "200 2", "300 1"), out);
    }

    public void testValues() {
        CellSink<Integer> b = new CellSink<Integer>(9);
        List<Integer> out = new ArrayList<Integer>();
        Listener l = b.listen(x -> {
            out.add(x);
        });
        b.send(2);
        b.send(7);
        l.unlisten();
        assertEquals(Arrays.asList(9, 2, 7), out);
    }

    public void testConstantBehavior() {
        Cell<Integer> b = new Cell<Integer>(12);
        List<Integer> out = new ArrayList();
        Listener l = b.listen(x -> {
            out.add(x);
        });
        l.unlisten();
        assertEquals(Arrays.asList(12), out);
    }

    public void testMapC() {
        CellSink<Integer> b = new CellSink<Integer>(6);
        List<String> out = new ArrayList<String>();
        Listener l = b.map(x -> x.toString())
                .listen(x -> {
                    out.add(x);
                });
        b.send(8);
        l.unlisten();
        assertEquals(Arrays.asList("6", "8"), out);
    }

    public void testMapCLateListen() {
        CellSink<Integer> b = new CellSink<Integer>(6);
        List<String> out = new ArrayList<String>();
        Cell<String> bm = b.map(x -> x.toString());
        b.send(2);
        Listener l = bm.listen(x -> {
            out.add(x);
        });
        b.send(8);
        l.unlisten();
        assertEquals(Arrays.asList("2", "8"), out);
    }

    public void testApply() {
        CellSink<Lambda1<Long, String>> bf = new CellSink<Lambda1<Long, String>>(
                (Long b) -> "1 " + b);
        CellSink<Long> ba = new CellSink<Long>(5L);
        List<String> out = new ArrayList<String>();
        Listener l = Cell.apply(bf, ba).listen(x -> {
            out.add(x);
        });
        bf.send((Long b) -> "12 " + b);
        ba.send(6L);
        l.unlisten();
        assertEquals(Arrays.asList("1 5", "12 5", "12 6"), out);
    }

    public void testLift() {
        CellSink<Integer> a = new CellSink<Integer>(1);
        CellSink<Long> b = new CellSink<Long>(5L);
        List<String> out = new ArrayList<String>();
        Listener l = a.lift(b,
                (x, y) -> x + " " + y
        ).listen((String x) -> {
            out.add(x);
        });
        a.send(12);
        b.send(6L);
        l.unlisten();
        assertEquals(Arrays.asList("1 5", "12 5", "12 6"), out);
    }

    public void testLiftGlitch() {
        CellSink<Integer> a = new CellSink<Integer>(1);
        Cell<Integer> a3 = a.map((Integer x) -> x * 3);
        Cell<Integer> a5 = a.map((Integer x) -> x * 5);
        Cell<String> b = a3.lift(a5, (x, y) -> x + " " + y);
        List<String> out = new ArrayList<String>();
        Listener l = b.listen((String x) -> {
            out.add(x);
        });
        a.send(2);
        l.unlisten();
        assertEquals(Arrays.asList("3 5", "6 10"), out);
    }

    public void testLiftFromSimultaneous() {
        Tuple2<CellSink<Integer>, CellSink<Integer>> t = Transaction.run(() -> {
            CellSink<Integer> b1 = new CellSink<>(3);
            CellSink<Integer> b2 = new CellSink<>(5);
            b2.send(7);
            return new Tuple2<>(b1, b2);
        });
        CellSink<Integer> b1 = t.a;
        CellSink<Integer> b2 = t.b;
        List<Integer> out = new ArrayList<>();
        Listener l = b1.lift(b2, (x, y) -> x + y)
                .listen(x -> {
                    out.add(x);
                });
        l.unlisten();
        assertEquals(Arrays.asList(10), out);
    }

    public void testHoldIsDelayed() {
        StreamSink<Integer> e = new StreamSink<Integer>();
        Cell<Integer> h = e.hold(0);
        Stream<String> pair = e.snapshot(h, (a, b) -> a + " " + b);
        List<String> out = new ArrayList<String>();
        Listener l = pair.listen((String x) -> {
            out.add(x);
        });
        e.send(2);
        e.send(3);
        l.unlisten();
        assertEquals(Arrays.asList("2 0", "3 2"), out);
    }

    static class SB {
        SB(Optional<Character> a, Optional<Character> b, Optional<Cell<Character>> sw) {
            this.a = a;
            this.b = b;
            this.sw = sw;
        }

        Optional<Character> a;
        Optional<Character> b;
        Optional<Cell<Character>> sw;
    }

    public void testSwitchC() {
        StreamSink<SB> esb = new StreamSink();
        // Split each field out of SB so we can update multiple behaviours in a
        // single transaction.
        Cell<Character> ba = Stream.filterOptional(esb.map(s -> s.a)).hold('A');
        Cell<Character> bb = Stream.filterOptional(esb.map(s -> s.b)).hold('a');
        Cell<Cell<Character>> bsw = Stream.filterOptional(esb.map(s -> s.sw)).hold(ba);
        Cell<Character> bo = Cell.switchC(bsw);
        List<Character> out = new ArrayList<Character>();
        Listener l = bo.listen(c -> {
            out.add(c);
        });
        esb.send(new SB(Optional.of('B'), Optional.of('b'), Optional.empty()));
        esb.send(new SB(Optional.of('C'), Optional.of('c'), Optional.of(bb)));
        esb.send(new SB(Optional.of('D'), Optional.of('d'), Optional.empty()));
        esb.send(new SB(Optional.of('E'), Optional.of('e'), Optional.of(ba)));
        esb.send(new SB(Optional.of('F'), Optional.of('f'), Optional.empty()));
        esb.send(new SB(Optional.empty(), Optional.empty(), Optional.of(bb)));
        esb.send(new SB(Optional.empty(), Optional.empty(), Optional.of(ba)));
        esb.send(new SB(Optional.of('G'), Optional.of('g'), Optional.of(bb)));
        esb.send(new SB(Optional.of('H'), Optional.of('h'), Optional.of(ba)));
        esb.send(new SB(Optional.of('I'), Optional.of('i'), Optional.of(ba)));
        l.unlisten();
        assertEquals(Arrays.asList('A', 'B', 'c', 'd', 'E', 'F', 'f', 'F', 'g', 'H', 'I'), out);
    }

    static class SE {
        SE(Character a, Character b, Optional<Stream<Character>> sw) {
            this.a = a;
            this.b = b;
            this.sw = sw;
        }

        Character a;
        Character b;
        Optional<Stream<Character>> sw;
    }

    public void testSwitchS() {
        StreamSink<SE> ese = new StreamSink();
        Stream<Character> ea = ese.map(s -> s.a);
        Stream<Character> eb = ese.map(s -> s.b);
        Cell<Stream<Character>> bsw = Stream.filterOptional(ese.map(s -> s.sw)).hold(ea);
        List<Character> out = new ArrayList();
        Stream<Character> eo = Cell.switchS(bsw);
        Listener l = eo.listen(c -> {
            out.add(c);
        });
        ese.send(new SE('A', 'a', Optional.empty()));
        ese.send(new SE('B', 'b', Optional.empty()));
        ese.send(new SE('C', 'c', Optional.of(eb)));
        ese.send(new SE('D', 'd', Optional.empty()));
        ese.send(new SE('E', 'e', Optional.of(ea)));
        ese.send(new SE('F', 'f', Optional.empty()));
        ese.send(new SE('G', 'g', Optional.of(eb)));
        ese.send(new SE('H', 'h', Optional.of(ea)));
        ese.send(new SE('I', 'i', Optional.of(ea)));
        l.unlisten();
        assertEquals(Arrays.asList('A', 'B', 'C', 'd', 'e', 'F', 'G', 'h', 'I'), out);
    }

    static class SS2 {
        SS2() {
        }

        final StreamSink<Integer> s = new StreamSink<Integer>();
    }

    public void testSwitchSSimultaneous() {
        SS2 ss1 = new SS2();
        CellSink<SS2> css = new CellSink<SS2>(ss1);
        Stream<Integer> so = Cell.switchS(css.<Stream<Integer>>map(b -> b.s));
        List<Integer> out = new ArrayList();
        Listener l = so.listen(c -> {
            out.add(c);
        });
        SS2 ss3 = new SS2();
        SS2 ss4 = new SS2();
        SS2 ss2 = new SS2();
        ss1.s.send(0);
        ss1.s.send(1);
        ss1.s.send(2);
        css.send(ss2);
        ss1.s.send(7);
        ss2.s.send(3);
        ss2.s.send(4);
        ss3.s.send(2);
        css.send(ss3);
        ss3.s.send(5);
        ss3.s.send(6);
        ss3.s.send(7);
        Transaction.runVoid(() ->
        {
            ss3.s.send(8);
            css.send(ss4);
            ss4.s.send(2);
        });
        ss4.s.send(9);
        l.unlisten();
        assertEquals(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), out);
    }

    public void testLoopCell() {
        final StreamSink<Integer> sa = new StreamSink();
        Cell<Integer> sum_out = Transaction.<Cell<Integer>>run(() -> {
            CellLoop<Integer> sum = new CellLoop<Integer>();
            Cell<Integer> sum_out_ = sa.snapshot(sum, (x, y) -> x + y).hold(0);
            sum.loop(sum_out_);
            return sum_out_;
        });
        List<Integer> out = new ArrayList();
        Listener l = sum_out.listen(x -> {
            out.add(x);
        });
        sa.send(2);
        sa.send(3);
        sa.send(1);
        l.unlisten();
        assertEquals(Arrays.asList(0, 2, 5, 6), out);
        assertEquals((int) 6, (int) sum_out.sample());
    }

    public void testAccum() {
        StreamSink<Integer> sa = new StreamSink();
        List<Integer> out = new ArrayList();
        Cell<Integer> sum = sa.accum(100, (a, s) -> a + s);
        Listener l = sum.listen((x) -> {
            out.add(x);
        });
        sa.send(5);
        sa.send(7);
        sa.send(1);
        sa.send(2);
        sa.send(3);
        l.unlisten();
        assertEquals(Arrays.asList(100, 105, 112, 113, 115, 118), out);
    }

    public void testLoopValueSnapshot() {
        List<String> out = new ArrayList();
        Listener l = Transaction.run(() -> {
            Cell<String> a = new Cell("lettuce");
            CellLoop<String> b = new CellLoop();
            Stream<String> eSnap = Operational.value(a).snapshot(b, (String aa, String bb) -> aa + " " + bb);
            b.loop(new Cell<String>("cheese"));
            return eSnap.listen((x) -> {
                out.add(x);
            });
        });
        l.unlisten();
        assertEquals(Arrays.asList("lettuce cheese"), out);
    }

    public void testLoopValueHold() {
        List<String> out = new ArrayList();
        Cell<String> value = Transaction.<Cell<String>>run(() -> {
            CellLoop<String> a = new CellLoop();
            Cell<String> value_ = Operational.value(a).hold("onion");
            a.loop(new Cell<String>("cheese"));
            return value_;
        });
        StreamSink<Unit> eTick = new StreamSink();
        Listener l = eTick.snapshot(value).listen((x) -> {
            out.add(x);
        });
        eTick.send(Unit.UNIT);
        l.unlisten();
        assertEquals(Arrays.asList("cheese"), out);
    }

    public void testLiftLoop() {
        List<String> out = new ArrayList();
        CellSink<String> b = new CellSink("kettle");
        Cell<String> c = Transaction.<Cell<String>>run(() -> {
            CellLoop<String> a = new CellLoop();
            Cell<String> c_ = a.lift(b,
                    (aa, bb) -> aa + " " + bb);
            a.loop(new Cell<String>("tea"));
            return c_;
        });
        Listener l = c.listen((x) -> {
            out.add(x);
        });
        b.send("caddy");
        l.unlisten();
        assertEquals(Arrays.asList("tea kettle", "tea caddy"), out);
    }

    public void testSwitchAndDefer() {
        List<String> out = new ArrayList();
        StreamSink<Integer> si = new StreamSink();
        Listener l = Cell.switchS(si.map(i -> {
            Cell<String> c = new Cell<>("A" + i);
            return Operational.defer(Operational.value(c));
        }).hold(new Stream<String>())).listen(x -> {
            out.add(x);
        });
        si.send(2);
        si.send(4);
        l.unlisten();
        assertEquals(Arrays.asList("A2", "A4"), out);
    }
}

