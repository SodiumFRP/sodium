import nz.sodium.*;
import java.util.Optional;

public abstract class Value<A> {
    public abstract ValueOutput<A> construct(Stream<A> sWrite);

    public final <B> Value<B> map(Bijection<A,B> bij) {
        Value<A> va = this;
        return new Value<B>() {
            public ValueOutput<B> construct(Stream<B> sWriteB) {
                ValueOutput<A> out = va.construct(sWriteB.map(bij.fInv));
                return new ValueOutput<B>(
                    out.value.map(oa ->
                        oa.isPresent() ? Optional.of(bij.f.apply(oa.get()))
                                       : Optional.empty()),
                    out.cleanup);
            }
        };
    }

    public final <B> Value<B> lens(
        Lambda1<A, B> getter,
        Lambda2<A, B, A> setter)
    {
        Value<A> va = this;
        return new Value<B>() {
            public ValueOutput<B> construct(Stream<B> sWriteB) {
                return Transaction.run(() -> {
                    StreamLoop<A> sWriteA = new StreamLoop<>();
                    ValueOutput<A> out = va.construct(sWriteA);
                    Cell<Optional<A>> oa = out.value;
                    sWriteA.loop(Stream.filterOptional(
                        sWriteB.snapshot(oa, (wb, oa_) ->
                            oa_.isPresent()
                                ? Optional.of(setter.apply(oa_.get(), wb))
                                : Optional.empty()
                        )
                    ));
                    return new ValueOutput<B>(
                        oa.map(oa_ ->
                            oa_.isPresent()
                                ? Optional.of(getter.apply(oa_.get()))
                                : Optional.empty()),
                        out.cleanup
                    );
                });
            }
        };
    }
}

