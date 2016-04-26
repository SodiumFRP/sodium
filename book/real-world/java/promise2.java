import nz.sodium.*;
import java.util.ArrayList;

public class promise2 {
    public static void main(String[] args) {
        System.out.println("*** Simple test");
        {
            ArrayList<String> out = new ArrayList<>();
            StreamSink<String> sa = new StreamSink<>();
            Promise<String> pa = new Promise<>(sa);
            StreamSink<String> sb = new StreamSink<>();
            Promise<String> pb = new Promise<>(sb);
            Promise<String> p = pa.lift(pb, (a, b) -> a + " " + b);
            sa.send("Hello");
            p.thenDo(t -> System.out.println(t));
            sb.send("World");
        }
        System.out.println("*** Simultaneous case");
        {
            ArrayList<String> out = new ArrayList<>();
            StreamSink<String> sa = new StreamSink<>();
            Promise<String> pa = new Promise<>(sa);
            StreamSink<String> sb = new StreamSink<>();
            Promise<String> pb = new Promise<>(sb);
            Promise<String> p = pa.lift(pb, (a, b) -> a + " " + b);
            p.thenDo(t -> System.out.println(t));
            Transaction.runVoid(() -> {
                sa.send("Hello");
                sb.send("World");
            });
        }
    }
}
