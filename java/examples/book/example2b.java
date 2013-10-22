import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import sodium.*;
import fj.data.List;


public class example2b extends JFrame
{
    private Listener l = new Listener();
    private Behavior<List<Point>> path;

    public example2b(String title)
    {
        super(title);

        // An event of mouse presses
        EventSink<Point> eClick = new EventSink<Point>();
        addMouseListener(new MouseAdapter() {
            public void mousePressed(java.awt.event.MouseEvent ev) {
                eClick.send(new Point(ev.getX(), ev.getY()));
            }
        });

        // The path is the accumulation of the clicked points
        path = eClick.accum(List.<Point>nil(), (pt, pth) -> pth.cons(pt));

        // When the path is changed, we repaint the window.
        l = path.updates().listen(pt -> {
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
        List<Point> points = path.sample();
        {
            List<Point> p = points;
            while (p.isNotEmpty()) {
                int x = p.head().x;
                int y = p.head().y;
                g.drawLine(x-5, y, x+5, y);
                g.drawLine(x, y-5, x, y+5);
                p = p.tail();
            }
        }
        g.setColor(Color.red);
        {
            List<Point> p = points;
            while (p.isNotEmpty()) {
                List<Point> p2 = p.tail();
                if (p2.isNotEmpty()) {
                    g.drawLine(p.head().x, p.head().y, p2.head().x, p2.head().y);
                    p = p2;
                }
                else
                    break;
            }
        }
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

