package fridgets;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.MouseEvent;
import nz.sodium.*;

public class FrTranslate extends Fridget {
    public FrTranslate(Fridget fr, Cell<Dimension> offset) {
        super((size, sMouse, sKey, focus, idSupply) -> {
            Stream<MouseEvent> sMouseNew =
                sMouse.snapshot(offset, (e, o) ->
                    new MouseEvent(e.getComponent(), e.getID(),
                        e.getWhen(), e.getModifiers(),
                        e.getX() - o.width, e.getY() - o.height,
                        e.getClickCount(), e.isPopupTrigger()));
            Fridget.Output fo = fr.reify(size, sMouseNew,
                sKey, focus, idSupply);
            Cell<Drawable> drawableNew = fo.drawable.lift(offset,
                (dr, o) -> new Drawable() {
                    public void draw(Graphics g) {
                        g.translate(o.width, o.height);
                        dr.draw(g);
                        g.translate(-o.width, -o.height);
                    } });
            return new Fridget.Output(drawableNew,
                fo.desiredSize, fo.sChangeFocus);
        });
    }
}
