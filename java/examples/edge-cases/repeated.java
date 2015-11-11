import nz.sodium.*;

public class repeated {
    public static void main(String[] args) {
        StreamSink<Integer> sA = Transaction.run(() -> {
            StreamSink<Integer> sA_ = new StreamSink<>();
            StreamLoop<Integer> sB = new StreamLoop<>();
            Stream<Integer> sC = sA_.orElse(sB);
            sB.loop(sC.map(x -> x + 1).filter(x -> x < 10));
            sC.listen(c -> System.out.println(c));
            return sA_;
        });
        System.out.println("send 5");
        sA.send(5);
    }
}
