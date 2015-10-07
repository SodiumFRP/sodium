import nz.sodium.*;
import java.util.Optional;

public class BackEnd {
    public BackEnd() {}
    public final <A> Value<A> allocate(String name, A initA) {
        StreamJunction<A> j = new StreamJunction<>((l, r) -> l);
        StreamSink<A> s0 = new StreamSink<>();
        Listener l = j.out.listenWeak(a -> {
            new Thread(() -> {
                try { Thread.sleep(50); }
                        catch (InterruptedException e) {}
                System.out.println("BackEnd: "+name+" <- " +a);
                s0.send(a);
            }).start();
        });
        Cell<A> c = s0.addCleanup(l).hold(initA);
        return new Value<A>() {
            public ValueOutput<A> construct(Stream<A> sWrite) {
                CellSink<Optional<A>> recvd =
                                  new CellSink<>(Optional.empty());
                Listener l =
                    j.add(sWrite)
                    .append(
                        c.listen(a -> {
                            new Thread(() -> {
                                try { Thread.sleep(50); }
                                        catch (InterruptedException e) {}
                                System.out.println("BackEnd: "
                                                        +name+" -> " +a);
                                recvd.send(Optional.of(a));
                            }).start();
                        })
                    );
                return new ValueOutput<A>(recvd, l);
            }
        };
    }
}
