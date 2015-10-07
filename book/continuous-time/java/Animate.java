import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Toolkit;
import javax.swing.JFrame;
import javax.swing.JPanel;
import nz.sodium.*;
import nz.sodium.time.*;

public class Animate extends JPanel {
    public Animate(Animation anim, Dimension windowSize) {
        Point extents = new Point(windowSize.width/2, windowSize.height/2);
        this.drawable = Transaction.run(() ->
            Shapes.translate(
                anim.create(new SecondsTimerSystem(), extents),
                new Cell<>(extents)));
        this.windowSize = windowSize;
    }
    private final Cell<Drawable> drawable;
    private final Dimension windowSize;

    public Dimension getPreferredSize() { return windowSize; }

    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        drawable.sample().draw(g, windowSize.height, new Point(0,0), 1.0);
        Toolkit.getDefaultToolkit().sync();
    }

    public static void animate(String title, Animation anim) {
        JFrame frame = new JFrame(title);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        CellSink<Double> clock = new CellSink<>(0.0);
        StreamSink<Unit> sAlarm = new StreamSink<>();
        JPanel view = new Animate(anim, new Dimension(500, 350));
        frame.setContentPane(view);
        frame.pack();
        frame.setVisible(true);
        long t0 = System.currentTimeMillis();
        long tLast = t0;
        while (true) {
            long t = System.currentTimeMillis();
            long tIdeal = tLast + 15;
            long toWait = tIdeal - t;
            if (toWait > 0)
                try { Thread.sleep(toWait); }
                catch (InterruptedException e) {}
            Transaction.runVoid(() -> {});
            view.repaint(0);
            tLast = tIdeal;
        }
    }
}

