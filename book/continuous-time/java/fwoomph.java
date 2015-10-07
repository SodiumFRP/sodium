import java.awt.Color;
import nz.sodium.*;

public class fwoomph extends Shapes {
    public static void main(String[] args) {
        Animate.animate("fwoomph", (sys, extents) -> {
            Cell<Double> time = sys.time;
            double maxSize = 200.0;
            return scale(
                circle(Color.green),
                time.map(t -> {
                    double frac = t - Math.floor(t);
                    return (frac < 0.5 ? frac : 1.0 - frac) * maxSize;
                })
            );
        });
    }
}

