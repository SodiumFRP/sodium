package fridgets;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.Optional;
import nz.sodium.*;

public class FrButton extends Fridget {
    public FrButton(Cell<String> label) {
        this(label, new StreamLoop<Unit>());
    }
    private FrButton(Cell<String> label, StreamLoop<Unit> sClicked) {
        super((size, sMouse, sKey, focus, idSupply) -> {
            Stream<Unit> sPressed = Stream.filterOptional(
                sMouse.snapshot(size, (e, osz) ->
                    osz.isPresent() &&
                    e.getID() == MouseEvent.MOUSE_PRESSED
                        && e.getX() >= 2 && e.getX() < osz.get().width-2
                        && e.getY() >= 2 && e.getY() < osz.get().height-2
                    ? Optional.of(Unit.UNIT)
                    : Optional.empty()
                )
            );
            Stream<Unit> sReleased = Stream.filterOptional(
                sMouse.map(e -> e.getID() == MouseEvent.MOUSE_RELEASED
                    ? Optional.of(Unit.UNIT)
                    : Optional.empty()));
            Cell<Boolean> pressed =
                sPressed.map(u -> true)
                        .orElse(sReleased.map(u -> false))
                        .hold(false);
            sClicked.loop(sReleased.gate(pressed)); 
            Font font = new Font("Helvetica", Font.PLAIN, 13);
            Canvas c = new Canvas();
            FontMetrics fm = c.getFontMetrics(font);
            Cell<Dimension> desiredSize = label.map(label_ ->
                new Dimension(
                    fm.stringWidth(label_) + 14,
                    fm.getHeight() + 10));
            return new Output(
                label.lift(size, pressed,
                    (label_, osz, pressed_) -> new Drawable() {
                        public void draw(Graphics g) {
                            if (osz.isPresent()) {
                                Dimension sz = osz.get();
                                int w = fm.stringWidth(label_);
                                g.setColor(pressed_ ? Color.darkGray
                                                    : Color.lightGray);
                                g.fillRect(3, 3, sz.width-6, sz.height-6);
                                g.setColor(Color.black);
                                g.drawRect(2, 2, sz.width-5, sz.height-5);
                                int centerX = sz.width / 2;
                                g.setFont(font);
                                g.drawString(label_,
                                    (sz.width - w)/2,
                                    (sz.height - fm.getHeight())/2
                                            + fm.getAscent());
                            }
                        } }
                ),
                desiredSize,
                new Stream<Long>()
            );
        });
        this.sClicked = sClicked;
    }
    public final Stream<Unit> sClicked;
}

