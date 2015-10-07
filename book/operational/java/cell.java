import nz.sodium.*;

public class cell {
    public static void main(String[] args) {
        CellSink<Integer> x = new CellSink<>(0);
        Listener l = x.listen(x_ -> { System.out.println(x_); });
        x.send(10);
        x.send(20);
        x.send(30);
        l.unlisten();
    }
}
