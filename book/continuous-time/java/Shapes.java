import java.awt.Color;
import java.awt.Graphics;
import nz.sodium.*;

public class Shapes {
    public static Cell<Drawable> circle(Color color) {
        return new Cell<Drawable>(new Drawable() {
            public void draw(Graphics g, int ht, Point offset, double sc) {
                int rad = (int)sc;
                int x = (int)offset.x;
                int y = (int)offset.y;
                g.setColor(color);
                g.fillOval(x-rad, (ht-1-y)-rad, rad*2, rad*2);
                g.setColor(Color.black);
                g.drawOval(x-rad, (ht-1-y)-rad, rad*2, rad*2);
            }
        });
    }

    public static Cell<Drawable> scale(Cell<Drawable> drawable,
                                       Cell<Double> scale) {
        return drawable.lift(scale, (dr, newSc) -> new Drawable() {
            public void draw(Graphics g, int ht, Point offset, double sc) {
                dr.draw(g, ht, offset, sc * newSc);
            }
        });
    }

    public static Cell<Drawable> translate(Cell<Drawable> drawable,
                                           Cell<Point> offset) {
        return drawable.lift(offset, (dr, o) -> new Drawable() {
            public void draw(Graphics g, int ht, Point offset, double sc) {
                dr.draw(g, ht, offset.add(o.multiply(sc)), sc);
            }
        });
    }

    public static Cell<Drawable> over(Cell<Drawable> a, Cell<Drawable> b) {
        return a.lift(b, (dra, drb) -> new Drawable() {
            public void draw(Graphics g, int ht, Point offset, double sc) {
                drb.draw(g, ht, offset, sc);
                dra.draw(g, ht, offset, sc);
            }
        });
    }
}

