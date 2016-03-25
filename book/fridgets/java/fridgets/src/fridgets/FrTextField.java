package fridgets;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.Optional;
import nz.sodium.*;

class TextUpdate {
    TextUpdate(String txt, int newX) {
        this.txt = txt;
        this.newX = newX;
    }
    String txt;
    int newX;
};

public class FrTextField extends Fridget {
    public FrTextField(String initText) {
        this(initText, new CellLoop<String>());
    }
    private FrTextField(String initText, CellLoop<String> text) {
        super((size, sMouse, sKey, focus, idSupply) -> {
            Stream<Integer> sPressed = Stream.filterOptional(
                sMouse.snapshot(size, (e, osz) ->
                    osz.isPresent() &&
                    e.getID() == MouseEvent.MOUSE_PRESSED
                        && e.getX() >= 2 && e.getX() < osz.get().width-2
                        && e.getY() >= 2 && e.getY() < osz.get().height-2
                    ? Optional.of(e.getX() - 2)
                    : Optional.empty()
                )
            );
            CellLoop<Integer> x = new CellLoop<>();
            long myId = idSupply.get();
            Cell<Boolean> haveFocus = focus.map(f_id -> f_id == myId);
            Font font = new Font("Helvetica", Font.PLAIN, 13);
            Canvas c = new Canvas();
            FontMetrics fm = c.getFontMetrics(font);
            Stream<TextUpdate> sTextUpdate = Stream.filterOptional(
                sKey.gate(haveFocus)
                    .snapshot(text, (key, txt) -> {
                        int x_ = x.sample();
                        if (key.getKeyChar() == (char)8) {
                            if (x_ > 0)
                                return Optional.of(new TextUpdate(
                                    txt.substring(0,x_-1)+
                                    txt.substring(x_),
                                    x_-1));
                            else
                                return Optional.empty();
                        }
                        else {
                            char[] keyChs = new char[1];
                            keyChs[0] = key.getKeyChar();
                            return Optional.of(new TextUpdate(
                                txt.substring(0, x_)+
                                new String(keyChs)+
                                txt.substring(x_),
                                x_ + 1));
                        }
                    })
            );
            x.loop(sPressed.snapshot(text,
                (xCoord, txt) -> {
                    for (int x_ = 1; x_ <= txt.length(); x_++)
                        if (xCoord < fm.stringWidth(txt.substring(0, x_)))
                            return x_-1;
                    return txt.length();
                })
                .orElse(sTextUpdate.map(tu -> tu.newX))
                .hold(0));
            text.loop(sTextUpdate.map(tu -> tu.txt).hold(initText));
            Cell<Dimension> desiredSize = text.map(txt ->
                new Dimension(
                    fm.stringWidth(txt) + 14,
                    fm.getHeight() + 10));
            return new Output(
                text.lift(x, haveFocus, size,
                    (txt, x_, haveFocus_, osz) -> new Drawable() {
                        public void draw(Graphics g) {
                            if (osz.isPresent()) {
                                Dimension sz = osz.get();
                                g.setColor(Color.white);
                                g.fillRect(3, 3, sz.width-6, sz.height-6);
                                g.setColor(Color.black);
                                g.drawRect(2, 2, sz.width-5, sz.height-5);
                                int centerX = sz.width / 2;
                                g.setFont(font);
                                int cursorX = fm.stringWidth(
                                    txt.substring(0, x_));
                                g.drawString(txt,
                                    4,
                                    (sz.height - fm.getHeight())/2
                                            + fm.getAscent());
                                if (haveFocus_) {
                                    g.setColor(Color.red);
                                    g.drawLine(4 + cursorX, 4,
                                               4 + cursorX, sz.height - 5);
                                }
                            }
                        } }
                ),
                desiredSize,
                sPressed.map(xCoord -> myId)
            );
        });
        this.text = text;
    }
    public final Cell<String> text;
}

