import java.awt.Color;
import nz.sodium.*;

public class cross extends Shapes {
    public static void main(String[] args) {
        Animate.animate("cross", (sys, extents) -> {
            Cell<Double> time = sys.time;
            double maxSize = 120;
            Cell<Double> offset = time.map(t -> {
                double frac = t - Math.floor(t);
                return (frac < 0.5 ? frac - 0.25 : 0.75 - frac)
                    * 4.0 * maxSize;
            });
            Cell<Double> fifty = new Cell<>(50.0);
            Cell<Drawable> greenBall = translate(
                scale(circle(Color.green), fifty),
                offset.map(x -> new Point(x, 0.0)));
            Cell<Drawable> blueBall = translate(
                scale(circle(Color.blue), fifty),
                offset.map(y -> new Point(0.0, y)));
            return over(greenBall, blueBall);
        });
    }
}

