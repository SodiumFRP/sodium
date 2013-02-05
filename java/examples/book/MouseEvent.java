import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionAdapter;
import java.awt.Component;
import java.awt.Point;
import sodium.*;

class MouseEvent {
    public enum Button { DOWN, UP, NONE };

    MouseEvent(Button left, Button right, Point pt) {
        this.left = left;
        this.right = right;
        this.pt = pt;
    }
    public Button left;
    public Button right;
    public Point pt;

    public String toString() { return left+" "+right+" "+pt; }

    public static Event<MouseEvent> mouseEventOf(Component c)
    {
        final EventSink<MouseEvent> e = new EventSink<MouseEvent>();
        c.addMouseListener(new MouseAdapter() {
            public void mousePressed(java.awt.event.MouseEvent ev) {
                e.send(new MouseEvent(
                    ev.getButton() == java.awt.event.MouseEvent.BUTTON1 ? MouseEvent.Button.DOWN : MouseEvent.Button.NONE,
                    ev.getButton() == java.awt.event.MouseEvent.BUTTON3 ? MouseEvent.Button.DOWN : MouseEvent.Button.NONE,
                    new Point(ev.getX(), ev.getY())));
            }
            public void mouseReleased(java.awt.event.MouseEvent ev) {
                e.send(new MouseEvent(
                    ev.getButton() == java.awt.event.MouseEvent.BUTTON1 ? MouseEvent.Button.UP : MouseEvent.Button.NONE,
                    ev.getButton() == java.awt.event.MouseEvent.BUTTON3 ? MouseEvent.Button.UP : MouseEvent.Button.NONE,
                    new Point(ev.getX(), ev.getY())));
            }
        });
        c.addMouseMotionListener(new MouseMotionAdapter() {
            public void mouseMoved(java.awt.event.MouseEvent ev) {
                e.send(new MouseEvent(MouseEvent.Button.NONE, MouseEvent.Button.NONE,
                    new Point(ev.getX(), ev.getY())));
            }
            public void mouseDragged(java.awt.event.MouseEvent ev) {
                e.send(new MouseEvent(MouseEvent.Button.NONE, MouseEvent.Button.NONE,
                    new Point(ev.getX(), ev.getY())));
            }
        });
        return e;
    }
}

