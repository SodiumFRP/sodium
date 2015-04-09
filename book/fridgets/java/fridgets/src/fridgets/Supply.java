package fridgets;

import java.util.Optional;

public class Supply {
    private static class Impl {
        private long nextID = 0;
        public final synchronized long alloc() { return ++nextID; }
    }
    public Supply() { this.impl = new Impl(); }
    private Supply(Impl impl) { this.impl = impl; }
    private final Impl impl;
    private Optional<Long> oID = Optional.empty();
    private Optional<Supply> oChild = Optional.empty();
    public final synchronized long get() {
        if (!oID.isPresent())
            oID = Optional.of(impl.alloc());
        return oID.get();
    }
    public final synchronized Supply child() {
        if (!oChild.isPresent())
            oChild = Optional.of(new Supply(impl));
        return oChild.get();
    }
}

