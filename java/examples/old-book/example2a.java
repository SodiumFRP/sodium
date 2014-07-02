import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import sodium.*;


public class example2a extends JFrame
{
    private Listener l = new Listener();
    private Behavior<Point> pos;

    public example2a(String title)
    {
        super(title);

        // An event of mouse presses
        EventSink<Point> eClick = new EventSink<Point>();
        addMouseListener(new MouseAdapter() {
            public void mousePressed(java.awt.event.MouseEvent ev) {
                eClick.send(new Point(ev.getX(), ev.getY()));
            }
        });

        // When the mouse is clicked, change the cross position.
        pos = eClick.hold(new Point(100, 100));

        // When the cross position is changed, we repaint the window.
        l = pos.updates().listen(pt -> {
            this.repaint();
        });
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
        example2a view = new example2a("example2a");
        view.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        view.setSize(700, 500);
        view.setVisible(true);
    }
}

