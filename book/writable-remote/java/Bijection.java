import nz.sodium.Lambda1;

public class Bijection<A,B> {
    public Bijection(Lambda1<A,B> f, Lambda1<B,A> fInv) {
        this.f = f;
        this.fInv = fInv;
    }
    public final Lambda1<A,B> f;
    public final Lambda1<B,A> fInv;
}

