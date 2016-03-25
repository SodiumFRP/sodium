import nz.sodium.*;
import java.util.Collection;

public class CellJunction<A> extends Junction<Cell<A>, A> {
    static <A> Cell<A> combines(Collection<Cell<A>> in,
                                   A nullValue, Lambda2<A,A,A> combine) {
        Cell<A> cOut = new Cell<>(nullValue);
        for (Cell<A> c : in)
            cOut = cOut.lift(c, combine);
        return cOut;
    }
    public CellJunction(A nullValue, Lambda2<A,A,A> combine) {
        this.out = Cell.switchC(
            clients.map(cls -> combines(cls, nullValue, combine)));
    }
    public Cell<A> out;
}

