import nz.sodium.*;
import java.util.ArrayList;

public class promise1 {
    public static void main(String[] args) {
        System.out.println("*** test 1");
        {
            ArrayList<String> out = new ArrayList<>();
            StreamSink<String> s1 = new StreamSink<>();
            Promise<String> p1 = new Promise<>(s1);
            s1.send("Early");
            p1.thenDo(t -> System.out.println(t));
        }
        System.out.println("*** test 2");
        {
            ArrayList<String> out = new ArrayList<>();
            StreamSink<String> s1 = new StreamSink<>();
            Promise<String> p1 = new Promise<>(s1);
            p1.thenDo(t -> System.out.println(t));
            s1.send("Late");
        }
    }
}
