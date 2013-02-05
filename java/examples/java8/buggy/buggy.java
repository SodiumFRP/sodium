import javax.swing.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import sodium.*;


public class buggy extends JFrame
{
    private Listener l = new Listener();
    private Behavior<Double> angle;
    private Behavior<Point> pos;
    static final int width = 700;
    static final int height = 550;
    static final int middleX = width / 2;
    static final int edge = 30;

    public buggy(String title)
    {
        super(title);

        Event<MouseEvent> eMouse = MouseEvent.mouseEventOf(this);
        Behavior<Double> wheelOffset = eMouse.map(m -> 2.0 * (double)(m.x - middleX) / (double)middleX).hold(0.0);
        Behavior<Double> clock = Clock.mkClock();
        angle = integral(clock, wheelOffset, 0.0);

        Behavior<Boolean> leftPressed = pressed(eMouse.map(m -> m.left));
        Behavior<Boolean> rightPressed = pressed(eMouse.map(m -> m.right));
        Behavior<Double> accelerator = leftPressed.map(p -> p ? 50.0 : -25.0);
        Behavior<Double> brake = rightPressed.map(p -> p ? -150.0 : 0);
        Behavior<Double> acceleration = Behavior.lift((acc, br) -> acc+br, accelerator, brake);
        Behavior<Double> speed = integral(clock, acceleration, 0.0, 0, 50.0);

        Behavior<Double> dirX = angle.map(theta -> Math.sin(theta));
        Behavior<Double> dirY = angle.map(theta -> -Math.cos(theta));
        Behavior<Double> velX = Behavior.lift((dx, sp) -> dx * sp, dirX, speed);
        Behavior<Double> velY = Behavior.lift((dy, sp) -> dy * sp, dirY, speed);

        Behavior<Double> x = integral(clock, velX, (double)width * 0.5, (double)edge, (double)(width - edge));
        Behavior<Double> y = integral(clock, velY, (double)height * 0.9, (double)edge, (double)(height - edge));

        pos = Behavior.lift((x_, y_) -> new Point((int)Math.round(x_), (int)Math.round(y_)), x, y);

        l = l.append(clock.changes().listen(tick -> {
            SwingUtilities.invokeLater(() -> {
                this.repaint();
            });
        }));
    }

    public static Behavior<Boolean> pressed(Event<MouseEvent.Button> ebut)
    {
        return ebut.map(b -> {
            if (b == MouseEvent.Button.DOWN) return new Boolean(true); else
            if (b == MouseEvent.Button.UP)   return new Boolean(false); else
                return null;
        }).filterNotNull().hold(false);
    }

    public static Behavior<Double> integral(Behavior<Double> clock, Behavior<Double> x, double x0)
    {
        return integral(clock, x, x0, -1e6, 1e6);
    }

    public static Behavior<Double> integral(Behavior<Double> clock, Behavior<Double> x, double x0, final double i_min, final double i_max)
    {
        Event<Double> dt = clock.changes().snapshot(clock, (t1, t0) -> (t1 - t0));
        Event<Double> x_dt = dt.snapshot(x, (dt_, x_) -> dt_ * x_);
        return x_dt.accum(x0, (x_dt_, i) -> {
            double new_i = x_dt_ + i;
            if (new_i < i_min) new_i = i_min;
            if (new_i > i_max) new_i = i_max;
            return new_i;
        }).hold(x0);
    }

    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }

    static final int[] xpts = {-6, -2,  2, 4, 7, 8, 6, 6, 4, 4, -2, -2, -4, -4, -6};
    static final int[] ypts = { 0, -4, -4, 0, 0, 4, 4, 6, 6, 4,  4,  6 , 6,  4,  4};

    public void paint(Graphics g_)
    {
        Graphics2D g = (Graphics2D)g_;
        g.setTransform(getGraphicsConfiguration().getDefaultTransform());
        g.setColor(getBackground());
        g.fillRect (0, 0, getWidth(), getHeight());
        g.setColor(Color.black);
        double theta = angle.sample();
        Point pt = pos.sample();
        g.translate(pt.x, pt.y);
        g.scale(2, 2);
        g.rotate(theta - Math.PI / 2);
        int x = 0;
        int y = 0;
        g.fillPolygon(xpts, ypts, xpts.length);
    }

    public static void main(String[] args)
    {
        buggy view = new buggy("buggy");
        view.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        view.setSize(width, height);
        view.setVisible(true);
    }
}

