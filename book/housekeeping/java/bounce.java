import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import sodium.*;

public class bounce extends JPanel {
    static enum Orientation { HORIZONTAL, VERTICAL };
    static class Wall {
        Wall(Orientation o, int coord) { this.o = o;
                                         this.coord = coord; }
        Orientation o;
        int coord;
    }
    static double gravity = 1000;
    static double restitution = 0.95;
    static class Position {
        Position(double x, double y, double vx, double vy) {
           this.x = x; this.y = y; this.vx = vx; this.vy = vy; }
        double x, y, vx, vy;
        Point toPoint() { return new Point((int)x, (int)y); }
        Position bounced(Orientation b) {
            if (b == Orientation.VERTICAL)
                return new Position(x, y, -vx*restitution, vy*restitution);
            else
                return new Position(x, y, vx*restitution, -vy*restitution);
        }
    }
    static class State {
        State(double t0, Position pos0, Wall[] walls) {
            this.t0 = t0;
            this.pos0 = pos0;
            double dtAlarm = 1000000.0;
            double x = pos0.x, y = pos0.y, vx = pos0.vx, vy = pos0.vy;
            final double quantum = 0.000001;
            Orientation bounce = Orientation.HORIZONTAL;
            for (Wall w : walls) {
                if (w.o == Orientation.VERTICAL) {
                    double dt = (w.coord - x) / vx;
                    if (dt >= quantum && dt < dtAlarm) {
                        dtAlarm = dt; bounce = w.o; }
                }
                else {
                    double a = gravity * 0.5, b = vy, c = y - w.coord;
                    double b24ac = Math.sqrt(b*b - 4*a*c);
                    double dt1 = ((-b) + b24ac) / (2*a);
                    double dt2 = ((-b) - b24ac) / (2*a);
                    if (dt1 >= quantum && dt1 < dtAlarm) {
                        dtAlarm = dt1; bounce = w.o; }
                    if (dt2 >= quantum && dt2 < dtAlarm) {
                        dtAlarm = dt2; bounce = w.o; }
                }
            }
            this.tAlarm = dtAlarm + t0;
            this.bounceOrient = bounce;
        }
        final double t0;
        final Position pos0;
        final double tAlarm;
        final Orientation bounceOrient;
        Position at(double t) {
            double dt = t - t0;
            double x = pos0.x, y = pos0.y, vx = pos0.vx, vy = pos0.vy;
            return new Position(vx * (t - t0) + x,
                                gravity * 0.5 * dt*dt + vy * dt + y,
                                vx,
                                vy + gravity * dt);
        }
    }
    bounce(Cell<Double> clock, Stream<Unit> sAlarm, Dimension windowSize,
                                                    Wall[] walls) {
        this.windowSize = windowSize;
        this.walls = walls;
        CellLoop<State> state = new CellLoop<>();
        Cell<Position> pos = Cell.lift((st, t) -> st.at(t), state, clock);
        Cell<Orientation> bounceOrient = state.map(st -> st.bounceOrient);
        state.loop(sAlarm.map(u -> {
            double t = clock.sample();
            Position p = pos.sample();
            Orientation bo = bounceOrient.sample();
            return new State(t, p.bounced(bo), walls);
        }).hold(
            new State(clock.sample(),
                      new Position(100, 0, 400, 0), walls)
        ));
        this.tAlarm = state.map(st -> st.tAlarm);
        this.ball = pos.map(p -> p.toPoint());
    }
    final Dimension windowSize;
    final Wall[] walls;
    final Cell<Point> ball;
    final Cell<Double> tAlarm;

    public Dimension getPreferredSize() { return windowSize; }
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        g.setColor(Color.darkGray);
        for (Wall w : walls) {
            if (w.o == Orientation.HORIZONTAL)
                g.drawLine(0, w.coord, windowSize.width, w.coord);
            else
                g.drawLine(w.coord, 0, w.coord, windowSize.height);
        }
        Point pt = ball.sample();
        g.setColor(Color.green);
        g.fillOval(pt.x-12, pt.y-12, 24, 24);
        g.setColor(Color.black);
        g.drawOval(pt.x-12, pt.y-12, 24, 24);
        Toolkit.getDefaultToolkit().sync();
    }

    public static void main(String[] args)
    {
        JFrame frame = new JFrame("bounce");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        CellSink<Double> clock = new CellSink<>(0.0);
        StreamSink<Unit> sAlarm = new StreamSink<>();
        Wall[] walls = new Wall[] { new Wall(Orientation.VERTICAL, 30),
                                    new Wall(Orientation.VERTICAL, 470),
                                    new Wall(Orientation.HORIZONTAL, 320)};
        bounce view = Transaction.run(() ->
            new bounce(clock, sAlarm, new Dimension(500, 350), walls));
        frame.setContentPane(view);
        frame.pack();
        frame.setVisible(true);
        long t0 = System.currentTimeMillis();
        long tLast = t0;
        while (true) {
            long t = System.currentTimeMillis();
            long tIdeal = tLast + 20;
            long toWait = tIdeal - t;
            if (toWait > 0)
                try { Thread.sleep(toWait); }
                catch (InterruptedException e) {}
            double tAnim = (double)(tIdeal - t0) * 0.001;
            while (true) {
                double tAl = view.tAlarm.sample();
                if (tAnim >= tAl) {
                    clock.send(tAl);
                    sAlarm.send(Unit.UNIT);
                }
                else
                    break;
            }
            clock.send(tAnim);
            view.repaint(0);
            tLast = tIdeal;
        }
    }
}

