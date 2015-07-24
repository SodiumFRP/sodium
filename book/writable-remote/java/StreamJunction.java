import sodium.*;
import java.util.Collection;

public class StreamJunction<A> extends Junction<Stream<A>, A> {
    public StreamJunction() {
        this.out = Cell.switchS(clients.map(cls -> Stream.merge(cls)));
    }
    public Stream<A> out;
}

