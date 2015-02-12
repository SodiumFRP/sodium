import javax.imageio.*;
import javax.swing.*;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.image.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import sodium.*;

public class Animate extends JPanel {
    private final double t0 = 0.0;
    private final Dimension windowSize = new Dimension(600, 400);
    private final BufferedImage sapienImgL;
    private final BufferedImage sapienImgR;
    private final BufferedImage zombicusImgL;
    private final BufferedImage zombicusImgR;
    private final BufferedImage coneImg;
    private Cell<List<Character>> characters;
    private CellSink<Double> clock;
    private StreamSink<Unit> sTick;

    public interface Animation {
        public Cell<List<Character>> create(
            double t0, Cell<Double> clock, Stream<Unit> sTick,
            Dimension screenSize);
    }

    public Animate(Animation animation)
        throws MalformedURLException, IOException
    {
        URL rootURL = new URL("file:.");
        sapienImgL = ImageIO.read(new URL(rootURL, "../images/homo-sapien-left.png"));
        sapienImgR = ImageIO.read(new URL(rootURL, "../images/homo-sapien-right.png"));
        zombicusImgL = ImageIO.read(new URL(rootURL, "../images/homo-zombicus-left.png"));
        zombicusImgR = ImageIO.read(new URL(rootURL, "../images/homo-zombicus-right.png"));
        coneImg = ImageIO.read(new URL(rootURL, "../images/roadius-conium.png"));
        Transaction.runVoid(() -> {
            clock = new CellSink<Double>(t0);
            sTick = new StreamSink<Unit>();
            this.characters = animation.create(t0, clock, sTick, windowSize);
        });
    }

    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Transaction.runVoid(() -> {
            List<Character> chars = new ArrayList<Character>(characters.sample());
            chars.sort((a, b) -> a.pos.y == b.pos.y ? 0 :
                                 a.pos.y < b.pos.y ? -1 : 1);
            for (Character c : chars) {
                if (c.type == CharacterType.SAPIEN)
                    if (c.velocity.dx < 0)
                        g.drawImage(sapienImgL, c.pos.x-30, c.pos.y-73, null);
                    else
                        g.drawImage(sapienImgR, c.pos.x-23, c.pos.y-73, null);
                else
                    if (c.velocity.dx < 0)
                        g.drawImage(zombicusImgL, c.pos.x-39, c.pos.y-73, null);
                    else
                        g.drawImage(zombicusImgR, c.pos.x-23, c.pos.y-73, null);
            }
        });
        Toolkit.getDefaultToolkit().sync();
    }
    
    public Dimension getPreferredSize() {
        return windowSize;
    }

    public static void animate(String title, Animation animation)
    {
        try {
            JFrame frame = new JFrame(title);
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            Animate view = new Animate(animation);
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
                    try { Thread.sleep(toWait); } catch (InterruptedException e) {}
                view.clock.send((double)(tIdeal - t0) * 0.001);
                view.sTick.send(Unit.UNIT);
                view.repaint(0);
                tLast = tIdeal;
            }
        }
        catch (MalformedURLException e) {
            e.printStackTrace();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}

