import sodium.*;
import java.util.Collection;

public class StreamJunction<A> extends Junction<Stream<A>, A> {
    static <A> Stream<A> merges(Collection<Stream<A>> in) {
        Stream<A> sOut = new Stream<>();
        for (Stream<A> c : in)
            sOut = sOut.merge(c);
        return sOut;
    }
    public StreamJunction() {
        this.out = Cell.switchS(clients.map(cls -> merges(cls)));
    }
    public Stream<A> out;
}

