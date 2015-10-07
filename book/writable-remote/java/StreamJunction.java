import nz.sodium.*;
import java.util.Collection;

public class StreamJunction<A> extends Junction<Stream<A>, A> {
    public StreamJunction(Lambda2<A,A,A> combine) {
        this.out = Cell.switchS(clients.map(cls ->
            Stream.merge(cls, combine)));
    }
    public Stream<A> out;
}

