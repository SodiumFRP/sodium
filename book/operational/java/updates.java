import nz.sodium.*;

public class updates {
    public static void main(String[] args) {
        CellSink<Integer> x = new CellSink<>(0);
        x.send(1);
        Listener l = Operational.updates(x).listen(x_ -> {
            System.out.println(x_);
        });
        x.send(2);
        x.send(3);
        l.unlisten();
    }
}
