package fridgets;

import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseEvent;
import java.awt.*;
import javax.swing.*;
import java.util.Optional;
import sodium.*;

public class FrView extends JPanel {
    public FrView(JFrame frame, Fridget fr) {
        StreamSink<MouseEvent> sMouse = new StreamSink<>();
        StreamSink<KeyEvent> sKey = new StreamSink<>();
        addMouseListener(new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                sMouse.send(e);
            }
            public void mouseReleased(MouseEvent e) {
                sMouse.send(e);
            }
        });
        addMouseMotionListener(new MouseMotionListener() {
            public void mouseDragged(MouseEvent e) {
                sMouse.send(e);
            }
            public void mouseMoved(MouseEvent e) {
                sMouse.send(e);
            }
        });
        size = new CellSink<Optional<Dimension>>(Optional.empty());
        addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent e) {
                if (e.getID() == ComponentEvent.COMPONENT_RESIZED)
                    size.send(Optional.of(getSize()));
            }
        });
        frame.addKeyListener(new KeyAdapter() {
            public void keyTyped(KeyEvent e) {
                System.out.println(e);
                sKey.send(e);
            }
        });
        Fridget.Output fo = fr.reify(size, sMouse, new Supply());
        this.drawable = fo.drawable;
        l = l.append(drawable.updates().listen(d -> {
            repaint();
        }));
    }

    private Listener l = new Listener();
    private final CellSink<Optional<Dimension>> size;
    private final Cell<Drawable> drawable;

    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        drawable.sample().draw(g);
    }
    public Dimension getPreferredSize() {
        return new Dimension(250, 300);
    }
    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }
    public void handleKeys(JFrame frame) {
    }
}

