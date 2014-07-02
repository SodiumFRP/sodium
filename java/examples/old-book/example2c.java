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
import fj.data.TreeMap;
import fj.Ord;


public class example2c extends JFrame
{
    private Listener l = new Listener();
    class Path {
        Path() {}
        Path(int nextID, TreeMap<Integer, Point> points) {
            this.nextID = nextID;
            this.points = points;
        }
        int nextID;
        TreeMap<Integer, Point> points = TreeMap.<Integer, Point>empty(Ord.intOrd);
    }
    private Behavior<Path> path;

    public example2c(String title)
    {
        super(title);

        Event<MouseEvent> eMouse = MouseEvent.mouseEventOf(this);

        BehaviorLoop<Path> path = new BehaviorLoop<Path>();
        this.path = path;

        Event<Point> ePressLeft = eMouse.map(m ->
            m.left == MouseEvent.Button.DOWN ? m.pt : null).filterNotNull(); 

        Event<Path> eAdded =
            ePressLeft.snapshot(path, (pt, p) -> new Path(p.nextID+1, p.points.set(p.nextID, pt)));

        path.loop(eAdded.hold(new Path()));

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
        List<Point> points = path.sample().points.values();
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
        example2c view = new example2c("example2c");
        view.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        view.setSize(700, 500);
        view.setVisible(true);
    }
}

