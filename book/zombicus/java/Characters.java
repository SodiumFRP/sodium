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

public class Characters extends JPanel {
    private final BufferedImage sapienImgL;
    private final BufferedImage sapienImgR;
    private final BufferedImage zombicusImgL;
    private final BufferedImage zombicusImgR;
    private final BufferedImage coneImg;
    private Cell<List<Character>> characters;
    private CellSink<Double> clock;
    private StreamSink<Unit> sTick;

    public Characters()
        throws MalformedURLException, IOException
    {
        URL rootURL = new URL("file:.");
        sapienImgL = ImageIO.read(new URL(rootURL, "../images/homo-sapien-left.png"));
        sapienImgR = ImageIO.read(new URL(rootURL, "../images/homo-sapien-right.png"));
        zombicusImgL = ImageIO.read(new URL(rootURL, "../images/homo-zombicus-left.png"));
        zombicusImgR = ImageIO.read(new URL(rootURL, "../images/homo-zombicus-right.png"));
        coneImg = ImageIO.read(new URL(rootURL, "../images/roadius-conium.png"));
        World world = new World(new Point(600, 400));
        Transaction.runVoid(() -> {
            double t0 = 0.0;
            clock = new CellSink<Double>(t0);
            sTick = new StreamSink<Unit>();
            List<Cell<Character>> chars = new ArrayList<>();
            CellLoop<List<Character>> others = new CellLoop<>();
            int id = 0;
            for (int x = 100; x < 600; x += 100)
                for (int y = 150; y < 400; y += 150) {
                    Point pos0 = new Point(x, y);
                    if (id != 3)
                        chars.add(HomoSapien.create(world, id, t0, pos0, clock, sTick));
                    else
                        chars.add(HomoZombicus.create(world, id, t0, pos0, clock, sTick, others));
                    id++;
                }
            Cell<List<Character>> others_ = new Cell<>(new ArrayList<Character>());
            for (Cell<Character> c : chars) {
                others_ = Cell.lift(
                    (cc, l0) -> {
                        List<Character> l = new ArrayList<Character>(l0);
                        l.add(cc);
                        return l;
                    },
                    c, others_);
            }
            this.characters = others_;
            List<Character> emptyOthers = new ArrayList<>();
            Cell<List<Character>> othersFixed = others_.updates().hold(emptyOthers);
            others.loop(othersFixed);
        });
    }

    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Transaction.runVoid(() -> {
            List<Character> chars = characters.sample();
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
        return new Dimension(600, 400);
    }

    public static void main(String[] args)
        throws MalformedURLException, IOException
    {
        JFrame frame = new JFrame("Zombicus characters");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        Characters view = new Characters();
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
            System.out.println(tIdeal);
            view.clock.send((double)(tIdeal - t0) * 0.001);
            view.sTick.send(Unit.UNIT);
            view.repaint(0);
            tLast = tIdeal;
        }
    }
}

