import nz.sodium.*;
import java.util.List;
import java.util.Arrays;

public class split {
    public static void main(String[] args) {
        StreamSink<List<Integer>> as = new StreamSink<>();
        Listener l = Operational.updates(
                Operational.split(as)
                      .<Integer>accum(0, (a, b) -> a + b)
            ).listen(total -> { System.out.println(total); });
        as.send(Arrays.asList(100, 15, 60));
        as.send(Arrays.asList(1, 5));
        l.unlisten();
    }
}

