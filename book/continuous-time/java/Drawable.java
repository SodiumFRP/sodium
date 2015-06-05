import java.awt.Graphics;

public class Drawable {
    public void draw(Graphics g, int ht, Point orig, double scale) {}
    public final Drawable append(Drawable second) {
        Drawable first = this;
        return new Drawable() {
            public void draw(Graphics g, int ht, Point orig, double sc) {
                first.draw(g, ht, orig, sc);
                second.draw(g, ht, orig, sc);
            }
        };
    }
}

