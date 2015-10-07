import java.util.Vector;
import java.util.TreeSet;
import java.util.Set;
import nz.sodium.*;

public class Helper {
    public static <A> Stream<Set<A>> mergeToSet(Iterable<Stream<A>> sa) {
        Vector<Stream<Set<A>>> asSets = new Vector<>();
        for (Stream<A> s : sa)
            asSets.add(s.map(a -> {
                TreeSet<A> set = new TreeSet<>();
                set.add(a);
                return set;
            }));
        return Stream.merge(asSets, (s1, s2) -> {
            TreeSet<A> out = new TreeSet<>(s1);
            out.addAll(s2);
            return out;
        });
    }
}

