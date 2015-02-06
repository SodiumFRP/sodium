import sodium.*;

public class glitch {
    public static void main(String[] args) {
        CellSink<Integer> ones = new CellSink<>(1);
        Cell<Integer> hundreds = ones.map(o -> o * 100);
        Cell<Integer> sum = Cell.lift((o, h) -> o + h, ones, hundreds);
        Listener l = sum.value().listen(s -> System.out.println(s));
        ones.send(2);
        l.unlisten();
    }
}
