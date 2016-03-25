import nz.sodium.*;
import java.util.Optional;

public class PromiseWithoutUpdates<A> {
    public PromiseWithoutUpdates(Stream<A> sDeliver) {
        this.sDeliver = sDeliver.once();
        this.oValue = this.sDeliver.map(a -> Optional.of(a))
                                   .hold(Optional.empty());
    }
    private PromiseWithoutUpdates(Stream<A> sDeliver, Cell<Optional<A>> oValue) {
        this.sDeliver = sDeliver;
        this.oValue = oValue;
    }
    public final Stream<A> sDeliver;
    public final Cell<Optional<A>> oValue;
    public final Stream<A> then() {
        return Stream.filterOptional(Operational.value(oValue))
            .orElse(sDeliver).once();
    }
    public final void thenDo(Handler<A> h) {
        Transaction.runVoid(() ->
            then().listenOnce(h)
        );
    }
    public static <A,B,C> PromiseWithoutUpdates<C> lift(final Lambda2<A, B, C> f,
                      PromiseWithoutUpdates<A> pa, PromiseWithoutUpdates<B> pb) {
        return Transaction.run(() -> {
            class Tuple {
                Tuple(Optional<A> oa, Optional<B> ob) {
                    this.oa = oa;
                    this.ob = ob;
                }
                Optional<A> oa;
                Optional<B> ob;
            };
            Lambda2<Tuple,Tuple,Tuple> combine = (l, r) -> new Tuple(
                l.oa.isPresent() ? l.oa : r.oa,
                l.ob.isPresent() ? l.ob : r.ob);
            Lambda1<Tuple,Optional<C>> result = t ->
                t.oa.isPresent() && t.ob.isPresent()
                    ? Optional.of(f.apply(t.oa.get(), t.ob.get()))
                    : Optional.empty();
            Stream<Tuple> sA = pa.sDeliver.map(a ->
                new Tuple(Optional.of(a), Optional.empty()));
            Cell<Tuple> vA = pa.oValue.map(oa ->
                new Tuple(oa, Optional.empty()));
            Stream<Tuple> sB = pb.sDeliver.map(b ->
                new Tuple(Optional.empty(), Optional.of(b)));
            Cell<Tuple> vB = pb.oValue.map(ob ->
                new Tuple(Optional.empty(), ob));
            Stream<Tuple> sAArrives = sA.snapshot(vB, combine);
            Stream<Tuple> sBArrives = sB.snapshot(vA, combine);
            Stream<Tuple> sSimultaneous = sA.merge(sB, combine);
            Stream<C> sDeliver = Stream.filterOptional(
                    sAArrives.orElse(sBArrives)
                             .orElse(sSimultaneous)
                             .map(result)
                ).once();
            Cell<Optional<C>> oValue = vA.lift(vB,
                combine).map(result);
            return new PromiseWithoutUpdates<C>(sDeliver, oValue);
        });
    }
}
