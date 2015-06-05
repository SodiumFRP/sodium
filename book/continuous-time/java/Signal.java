import sodium.*;
import java.util.Optional;

public class Signal {
    public Signal(double t0, double a, double b, double c) {
        this.t0 = t0;
        this.a = a;
        this.b = b;
        this.c = c;
    }
    public final double t0, a, b, c;
    public double at(double t) {
        double x = t - t0;
        return a*x*x + b*x + c;
    }
    public final static double quantum = 0.000001;
    public Optional<Double> when(double y) {
        double c = this.c - y;
        if (a == 0) {
            double x = (-c) / b;
            return x >= quantum ? Optional.of(x + t0)
                                : Optional.empty();
        }
        else {
            double b24ac = Math.sqrt(b*b - 4*a*c);
            double x1 = ((-b) + b24ac) / (2*a);
            double x2 = ((-b) - b24ac) / (2*a);
            return x1 >= quantum
                ? x2 >= quantum ? Optional.of((x1 < x2 ? x1 : x2) + t0)
                                : Optional.of(x1 + t0)
                : x2 >= quantum ? Optional.of(x2 + t0)
                                : Optional.empty();
        }
    }
    public Signal integrate(double initial) {
        if (a != 0.0) throw new InternalError("Signal can't handle x^3");
        return new Signal(t0, b/2, c, initial);
    }
    public static Cell<Signal> integrate(
                                    Cell<Signal> sig, double initial) {
        Stream<Signal> sSig = Operational.updates(sig);
        return sSig.accum(sig.sample().integrate(initial),
            (neu, old) -> neu.integrate(old.at(neu.t0)));
    }
}

