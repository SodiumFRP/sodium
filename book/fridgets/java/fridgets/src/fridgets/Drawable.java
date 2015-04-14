package fridgets;

import java.awt.Graphics;

public class Drawable {
    public void draw(Graphics g) {}
    public final Drawable append(Drawable second) {
        Drawable first = this;
        return new Drawable() {
            public void draw(Graphics g) {
                first.draw(g);
                second.draw(g);
            }
        };
    }
}
