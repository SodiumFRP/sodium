import nz.sodium.*;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public abstract class Junction<ContainerA, A> {
    private int nextID;
    private StreamSink<Lambda1<Map<Integer, ContainerA>,
                               Map<Integer, ContainerA>>> sUpdate
        = new StreamSink<>((f1, f2) -> a -> f1.apply(f2.apply(a)));
    protected Cell<Collection<ContainerA>> clients;
    public Junction() {
        clients = sUpdate
            .<Map<Integer, ContainerA>>accum(
                new HashMap<Integer, ContainerA>(),
                (f, m) -> f.apply(m))
            .map(m -> m.values());
    }
    public Listener add(ContainerA c) {
        int id;
        synchronized (this) {
            id = nextID++;
        }
        sUpdate.send(m0 -> {
            java.util.HashMap<Integer, ContainerA> m = new HashMap(m0);
            m.put(id, c);
            return m;
        });
        return new Listener() {
            public void unlisten() {
                sUpdate.send(m0 -> {
                    java.util.HashMap<Integer, ContainerA> m
                                                     = new HashMap(m0);
                    m.remove(id);
                    return m;
                });
            }
        };
    }
}

