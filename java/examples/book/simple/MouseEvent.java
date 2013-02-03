import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionAdapter;
import java.awt.Component;
import sodium.*;

class MouseEvent {
    public enum Type { DOWN, UP, MOVE };

    MouseEvent(Type type, int x, int y) {
        this.type = type;
        this.x = x;
        this.y = y;
    }
    public Type type;
    public int x;
    public int y;

    public String toString() { return type.toString()+" "+x+" "+y; }

    public static Event<MouseEvent> mouseEventOf(Component c)
    {
        final EventSink<MouseEvent> e = new EventSink<MouseEvent>();
        c.addMouseListener(new MouseAdapter() {
            public void mousePressed(java.awt.event.MouseEvent ev) {
                e.send(new MouseEvent(MouseEvent.Type.DOWN, ev.getX(), ev.getY()));
            }
            public void mouseReleased(java.awt.event.MouseEvent ev) {
                e.send(new MouseEvent(MouseEvent.Type.UP, ev.getX(), ev.getY()));
            }
        });
        c.addMouseMotionListener(new MouseMotionAdapter() {
            public void mouseMoved(java.awt.event.MouseEvent ev) {
                e.send(new MouseEvent(MouseEvent.Type.MOVE, ev.getX(), ev.getY()));
            }
            public void mouseDragged(java.awt.event.MouseEvent ev) {
                e.send(new MouseEvent(MouseEvent.Type.MOVE, ev.getX(), ev.getY()));
            }
        });
        return e;
    }
}

