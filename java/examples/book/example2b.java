import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import sodium.*;


public class example2b extends JFrame
{
    private Event<MouseEvent> eMouse;
    private Listener l = new Listener();
    private Behavior<Point> pos;

    public example2b(String title)
    {
        super(title);
        this.eMouse = MouseEvent.mouseEventOf(this);

        Event<Point> eClick = eMouse.map( m -> {
            return m.type == MouseEvent.Type.UP ? new Point(m.x, m.y) : null;
        }).filterNotNull();
        pos = eClick.hold(new Point(100, 100));

        l = l.append(pos.changes().listen(pt -> {
            this.repaint();
        }));
    }

    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }

    public void paint(Graphics g)
    {
        g.setColor(getBackground());
        g.fillRect (0, 0, getWidth(), getHeight());
        g.setColor(Color.black);
        Point pt = pos.sample();
        int x = pt.x;
        int y = pt.y;
        g.drawLine(x-5, y, x+5, y);
        g.drawLine(x, y-5, x, y+5);
    }

    public static void main(String[] args)
    {
        example2b view = new example2b("example2b");
        view.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        view.setSize(700, 500);
        view.setVisible(true);
    }
}

