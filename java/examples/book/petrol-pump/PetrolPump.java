import javax.swing.*;
import javax.imageio.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.Component;
import java.awt.Point;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.image.*;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Optional;
import java.util.List;
import sodium.*;


class PumpFace extends Component {
    private Listener l = new Listener();
    private final BufferedImage background;
    private final BufferedImage[] smalls = new BufferedImage[8];
    private final BufferedImage[] larges = new BufferedImage[8];
    private final BufferedImage[] nozzleImgs = new BufferedImage[3];
    private final Behavior<List<Integer>> presetLCD;
    private final Behavior<List<Integer>> saleCostLCD;
    private final Behavior<List<Integer>> saleQuantityLCD;
    private final Behavior<List<Integer>> priceLCD1;
    private final Behavior<List<Integer>> priceLCD2;
    private final Behavior<List<Integer>> priceLCD3;
    @SuppressWarnings("unchecked")
    private final Behavior<UpDown> nozzles[] = new Behavior[3];
    @SuppressWarnings("unchecked")
    public final Behavior<Rectangle>[] nozzleRects = new Behavior[3];
    PumpFace(
        URL rootURL,
        EventSink<Point> eClick,
        Behavior<List<Integer>> presetLCD,
        Behavior<List<Integer>> saleCostLCD,
        Behavior<List<Integer>> saleQuantityLCD,
        Behavior<List<Integer>> priceLCD1,
        Behavior<List<Integer>> priceLCD2,
        Behavior<List<Integer>> priceLCD3,
        Behavior<UpDown> nozzle1,
        Behavior<UpDown> nozzle2,
        Behavior<UpDown> nozzle3
    ) throws IOException {
        addMouseListener(new MouseAdapter() {
            public void mousePressed(java.awt.event.MouseEvent ev) {
                eClick.send(new Point(ev.getX(), ev.getY()));
            }
        });
        this.presetLCD = presetLCD;
        this.saleCostLCD = saleCostLCD;
        this.saleQuantityLCD = saleQuantityLCD;
        this.priceLCD1 = priceLCD1;
        this.priceLCD2 = priceLCD2;
        this.priceLCD3 = priceLCD3;
        this.nozzles[0] = nozzle1;
        this.nozzles[1] = nozzle2;
        this.nozzles[2] = nozzle3;
        l = l.append(presetLCD.updates().listen(text -> {
            this.repaintSegments(193, 140, larges, 5);
        })).append(saleCostLCD.updates().listen(text -> {
            this.repaintSegments(517, 30, larges, 5);
        })).append(saleQuantityLCD.updates().listen(text -> {
            this.repaintSegments(517, 120, larges, 5);
        })).append(priceLCD1.updates().listen(text -> {
            this.repaintSegments(355, 230, smalls, 4);
        })).append(priceLCD2.updates().listen(text -> {
            this.repaintSegments(485, 230, smalls, 4);
        })).append(priceLCD3.updates().listen(text -> {
            this.repaintSegments(615, 230, smalls, 4);
        })).append(nozzle1.updates().listen(ud -> {
            this.repaint();
        })).append(nozzle2.updates().listen(ud -> {
            this.repaint();
        })).append(nozzle3.updates().listen(ud -> {
            this.repaint();
        }));
        background = ImageIO.read(new URL(rootURL, "images/petrol-pump-front.png"));
        for (int i = 0; i < 8; i++) {
            smalls[i] = ImageIO.read(new URL(rootURL, "images/small"+i+".png"));
            larges[i] = ImageIO.read(new URL(rootURL, "images/large"+i+".png"));
        }
        for (int i = 0; i < 3; i++) {
            nozzleImgs[i] = ImageIO.read(new URL(rootURL, "images/nozzle"+(i+1)+".png"));
            final int x = 270 + i*130;
            final int width = nozzleImgs[i].getWidth(null);
            final int height = nozzleImgs[i].getHeight(null);
            nozzleRects[i] = nozzles[i].map(upDown ->
                new Rectangle(x, upDown == UpDown.UP ? 300 : 330, width, height)
            );
        }
    }
    @Override
    public Dimension getPreferredSize() {
        return new Dimension(background.getWidth(null), background.getHeight(null));
    }
    @Override
    public void paint(Graphics g) {
        g.drawImage(background, 0, 0, null);
        Transaction.run(() -> {
            drawSegments(g, 193, 140, presetLCD.sample(), larges, 5);
            drawSegments(g, 517, 30, saleCostLCD.sample(), larges, 5);
            drawSegments(g, 517, 120, saleQuantityLCD.sample(), larges, 5);
            drawSegments(g, 355, 230, priceLCD1.sample(), smalls, 4);
            drawSegments(g, 485, 230, priceLCD2.sample(), smalls, 4);
            drawSegments(g, 615, 230, priceLCD3.sample(), smalls, 4);
            for (int i = 0; i < 3; i++) {
                Rectangle r = nozzleRects[i].sample();
                g.drawImage(nozzleImgs[i], r.x, r.y, null);
            }
        });
    }
    @Override
    public void update(Graphics g) {
        paint(g);  // Don't clear the background, since we are painting the whole lot
    }

    private static Rectangle lcdBounds(int ox, int oy, BufferedImage[] images, int noOfDigits)
    {
        int w = images[0].getWidth(null);
        int h = images[0].getHeight(null);
        return new Rectangle(ox - w * noOfDigits, oy, w * noOfDigits, h);
    }

    private void repaintSegments(int ox, int oy, BufferedImage[] images, int noOfDigits)
    {
        Rectangle r = lcdBounds(ox, oy, images, noOfDigits);
        repaint(r.x, r.y, r.width, r.height);
    }

    public static void drawSegments(Graphics g, int ox, int oy, List<Integer> digits,
                                         BufferedImage[] images, int noOfDigits)
    {
        if (g.getClipBounds().intersects(lcdBounds(ox, oy, images, noOfDigits)))
            for (int i = 0; i < digits.size() && i < noOfDigits; i++) {
                int x = ox - images[0].getWidth(null)*(i+1);
                int digit = digits.get(i);
                for (int j = 0; j < 8; j++)
                    if ((digit & (1 << j)) != 0)
                        g.drawImage(images[j], x, oy, null); 
            }
    }
}

public class PetrolPump extends JFrame
{
    private Listener l = new Listener();
    private Event<Key> eKey;
    public BehaviorSink<List<Integer>> presetLCD;

    public PetrolPump(URL rootURL) throws IOException
    {
        super("Functional Reactive Petrol Pump");

        Transaction.run(() -> {
            try {
                // An event of mouse presses
                EventSink<Point> eClick = new EventSink<Point>();

                /*
                l = l.append(eClick.listen(pt -> {
                    System.out.println(pt);
                }));
                */

                eKey = toKey(eClick);

                l = l.append(eKey.listen(key -> {
                    System.out.println(key);
                }));

                Integer[] five = {0xff, 0xff, 0xff, 0xff, 0xff};
                List<Integer> five8s = Arrays.asList(five);
                Integer[] four = {0xff, 0xff, 0xff, 0xff};
                List<Integer> four8s = Arrays.asList(four);
                presetLCD = new BehaviorSink<>(five8s);
                Behavior<List<Integer>> saleCostLCD = new Behavior<>(five8s);
                Behavior<List<Integer>> saleQuantityLCD = new Behavior<>(five8s);
                Behavior<List<Integer>> priceLCD1 = new Behavior<>(four8s);
                Behavior<List<Integer>> priceLCD2 = new Behavior<>(four8s);
                Behavior<List<Integer>> priceLCD3 = new Behavior<>(four8s);
                @SuppressWarnings("unchecked")
                BehaviorLoop<UpDown>[] nozzles = new BehaviorLoop[3];
                for (int i = 0; i < 3; i++)
                    nozzles[i] = new BehaviorLoop<UpDown>();
                PumpFace face = new PumpFace(
                        rootURL, eClick,
                        presetLCD,
                        saleCostLCD,
                        saleQuantityLCD,
                        priceLCD1,
                        priceLCD2,
                        priceLCD3,
                        nozzles[0],
                        nozzles[1],
                        nozzles[2]
                    );
                add("Center", face);
                for (int i = 0; i < 3; i++) {
                    final Behavior<Tuple2<Rectangle, UpDown>> rect_state =
                        Behavior.lift(
                            (rect, state) -> new Tuple2<Rectangle, UpDown>(rect, state),
                            face.nozzleRects[i], nozzles[i]);
                    ((BehaviorLoop<UpDown>)nozzles[i]).loop(
                        Event.<UpDown>filterOptional(
                            eClick.snapshot(rect_state,
                                (pt, rs) -> rs.a.contains(pt) ? Optional.of(rs.b.invert())
                                                              : Optional.empty()
                            )
                        ).hold(UpDown.DOWN)
                    );
                }
            }
            catch (IOException e) {
                System.err.println("Unexpected exception: "+e);
            }
        });
        pack();
    }

    public static Event<Key> toKey(Event<Point> eClick) {
        HashMap<Tuple2<Integer,Integer>, Key> keys = new HashMap<>();
        keys.put(new Tuple2<>(0,0), Key.ONE);
        keys.put(new Tuple2<>(1,0), Key.TWO);
        keys.put(new Tuple2<>(2,0), Key.THREE);
        keys.put(new Tuple2<>(0,1), Key.FOUR);
        keys.put(new Tuple2<>(1,1), Key.FIVE);
        keys.put(new Tuple2<>(2,1), Key.SIX);
        keys.put(new Tuple2<>(0,2), Key.SEVEN);
        keys.put(new Tuple2<>(1,2), Key.EIGHT);
        keys.put(new Tuple2<>(2,2), Key.NINE);
        keys.put(new Tuple2<>(1,3), Key.ZERO);
        keys.put(new Tuple2<>(2,3), Key.CLEAR);

        return Event.filterOptional(eClick.map(pt -> {
            int x = pt.x - 40;
            int y = pt.y - 230;
            int col = x / 50;
            int row = y / 50;
            boolean valid =
                x >= 0 && x % 50 < 40 &&
                y >= 0 && y % 50 < 40 &&
                col < 3 && row < 4;
            Key key = valid ? keys.get(new Tuple2<>(col, row)) : null;
            return Optional.ofNullable(key);
        }));
    }

    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }

    public static void main(String[] args) throws MalformedURLException, IOException
    {
        URL rootURL = new URL("file:.");
        PetrolPump view = new PetrolPump(rootURL);
        view.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        view.setVisible(true);
        
        while (true) {
            for (int i = 0; i < 256; i++) {
                Integer[] digits = {i, i, i, i, i};
                view.presetLCD.send(Arrays.asList(digits));
                try { Thread.sleep(100); } catch (InterruptedException e) {}
            }
        }
    }
}

