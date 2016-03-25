package pump;

import javax.swing.*;
import javax.imageio.*;
import java.applet.Applet;
import java.applet.AudioClip;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.image.*;
import java.awt.Toolkit;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Optional;
import java.util.List;
import nz.sodium.*;


class PumpFace extends Component {
    private Listener l = new Listener();
    private final BufferedImage background;
    private final BufferedImage[] smalls = new BufferedImage[8];
    private final BufferedImage[] larges = new BufferedImage[8];
    private final BufferedImage[] nozzleImgs = new BufferedImage[3];
    private final Cell<List<Integer>> presetLCD;
    private final Cell<List<Integer>> saleCostLCD;
    private final Cell<List<Integer>> saleQuantityLCD;
    private final Cell<List<Integer>> priceLCD1;
    private final Cell<List<Integer>> priceLCD2;
    private final Cell<List<Integer>> priceLCD3;
    @SuppressWarnings("unchecked")
    private final Cell<UpDown> nozzles[] = new Cell[3];
    @SuppressWarnings("unchecked")
    public final Cell<Rectangle>[] nozzleRects = new Cell[3];
    PumpFace(
        URL rootURL,
        StreamSink<Point> sClick,
        Cell<List<Integer>> presetLCD,
        Cell<List<Integer>> saleCostLCD,
        Cell<List<Integer>> saleQuantityLCD,
        Cell<List<Integer>> priceLCD1,
        Cell<List<Integer>> priceLCD2,
        Cell<List<Integer>> priceLCD3,
        Cell<UpDown> nozzle1,
        Cell<UpDown> nozzle2,
        Cell<UpDown> nozzle3
    ) throws IOException {
        addMouseListener(new MouseAdapter() {
            public void mousePressed(java.awt.event.MouseEvent ev) {
                sClick.send(new Point(ev.getX(), ev.getY()));
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
        l = l.append(presetLCD.listen(text -> {
            this.repaintSegments(193, 140, larges, 5);
        })).append(saleCostLCD.listen(text -> {
            this.repaintSegments(517, 30, larges, 5);
        })).append(saleQuantityLCD.listen(text -> {
            this.repaintSegments(517, 120, larges, 5);
        })).append(priceLCD1.listen(text -> {
            this.repaintSegments(355, 230, smalls, 4);
        })).append(priceLCD2.listen(text -> {
            this.repaintSegments(485, 230, smalls, 4);
        })).append(priceLCD3.listen(text -> {
            this.repaintSegments(615, 230, smalls, 4);
        })).append(nozzle1.listen(ud -> {
            this.repaint(0);
        })).append(nozzle2.listen(ud -> {
            this.repaint(0);
        })).append(nozzle3.listen(ud -> {
            this.repaint(0);
        }));
        background = ImageIO.read(new URL(rootURL, "../images/petrol-pump-front.png"));
        for (int i = 0; i < 8; i++) {
            smalls[i] = ImageIO.read(new URL(rootURL, "../images/small"+i+".png"));
            larges[i] = ImageIO.read(new URL(rootURL, "../images/large"+i+".png"));
        }
        for (int i = 0; i < 3; i++) {
            nozzleImgs[i] = ImageIO.read(new URL(rootURL, "../images/nozzle"+(i+1)+".png"));
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
        Transaction.runVoid(() -> {
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
        Toolkit.getDefaultToolkit().sync();
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
        repaint(0, r.x, r.y, r.width, r.height);
    }

    public static void drawSegments(Graphics g, int ox, int oy, List<Integer> digits,
                                         BufferedImage[] images, int noOfDigits)
    {
        if (g.getClipBounds().intersects(lcdBounds(ox, oy, images, noOfDigits)))
            for (int i = 0; i < digits.size() && i < noOfDigits; i++) {
                int x = ox - images[0].getWidth(null)*(i+1);
                int digit = digits.get(digits.size() - 1 - i);
                for (int j = 0; j < 8; j++)
                    if ((digit & (1 << j)) != 0)
                        g.drawImage(images[j], x, oy, null); 
            }
    }
}

class ClassNameRenderer extends DefaultListCellRenderer {
    public ClassNameRenderer() {
        setOpaque(true);
    }

    public Component getListCellRendererComponent(JList<?> list,
                                                  Object value,
                                                  int index,
                                                  boolean isSelected,
                                                  boolean cellHasFocus) {
        return super.getListCellRendererComponent(list, value.getClass().getName(), index, isSelected, cellHasFocus);
    }
}

public class PetrolPump extends JFrame
{
    private Listener l = new Listener();
    private Stream<Key> sKey;
    public StreamSink<Integer> sFuelPulses = new StreamSink<>();
    public Cell<Delivery> delivery;

    private static Cell<List<Integer>> format7Seg(Cell<String> text, int digits)
    {
        return text.map(text_ -> {
            Integer[] segs = new Integer[digits];
            for (int i = 0; i < digits; i++)
                segs[i] = 0;
            int i = digits-1;
            int j = text_.length() - 1;
            while (j >= 0 && i >= 0) {
                char ch = text_.charAt(j);
                switch (ch) {
                    case '-': segs[i] |= 0x08; i--; break;
                    case '0': segs[i] |= 0x77; i--; break;
                    case '1': segs[i] |= 0x24; i--; break;
                    case '2': segs[i] |= 0x6b; i--; break;
                    case '3': segs[i] |= 0x6d; i--; break;
                    case '4': segs[i] |= 0x3c; i--; break;
                    case '5': segs[i] |= 0x5d; i--; break;
                    case '6': segs[i] |= 0x5f; i--; break;
                    case '7': segs[i] |= 0x64; i--; break;
                    case '8': segs[i] |= 0x7f; i--; break;
                    case '9': segs[i] |= 0x7c; i--; break;
                    case '.': segs[i] |= 0x80;
                }
                j--;
            }
            return Arrays.<Integer>asList(segs);
        });
    }

    public static <A> Stream<A> changes(Cell<A> b)
    {
        return Stream.filterOptional(
            Operational.value(b).snapshot(b, (neu, old) ->
                old.equals(neu) ? Optional.empty() : Optional.of(neu)));
    }

    @SuppressWarnings("unchecked")
    public PetrolPump(URL rootURL) throws IOException
    {
        super("Functional Reactive Petrol Pump");

        Transaction.runVoid(() -> {
            try {
                setLayout(new BorderLayout());

                Container topTwoPanels = new Container();
                add(topTwoPanels, BorderLayout.NORTH);
                topTwoPanels.setLayout(new GridLayout(0,1));
                Container firstPanel = new Container();
                firstPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
                topTwoPanels.add(firstPanel);
                Container secondPanel = new Container();
                secondPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
                topTwoPanels.add(secondPanel);
                firstPanel.add(new JLabel("Logic"));

                SComboBox<Pump> logic = new SComboBox<>(new DefaultComboBoxModel<Pump>(new Pump[] {
                    new chapter4.section4.LifeCyclePump(),
                    new chapter4.section6.AccumulatePulsesPump(),
                    new chapter4.section7.ShowDollarsPump(),
                    new chapter4.section8.ClearSalePump(),
                    new chapter4.section9.KeypadPump(),
                    new chapter4.section11.PresetAmountPump()
                }));
                logic.setRenderer(new ClassNameRenderer());
                firstPanel.add(logic);
                secondPanel.add(new JLabel("Price1"));
                STextField textPrice1 = new STextField("2.149", 7);
                secondPanel.add(textPrice1);
                secondPanel.add(new JLabel("Price2"));
                STextField textPrice2 = new STextField("2.341", 7);
                secondPanel.add(textPrice2);
                secondPanel.add(new JLabel("Price3"));
                STextField textPrice3 = new STextField("1.499", 7);
                secondPanel.add(textPrice3);
                
                Lambda1<String,Double> parseDbl = str -> {
                    try {
                        return Double.parseDouble(str);
                    }
                    catch (NumberFormatException e) {
                        return 0.0;
                    }
                };

                // An event of mouse presses
                StreamSink<Point> sClick = new StreamSink<Point>();

                /*
                l = l.append(sClick.listen(pt -> {
                    System.out.println(pt);
                }));
                */

                sKey = toKey(sClick);

                /*
                l = l.append(sKey.listen(key -> {
                    System.out.println(key);
                }));
                */

                Integer[] five = {0xff, 0xff, 0xff, 0xff, 0xff};
                List<Integer> five8s = Arrays.asList(five);
                Integer[] four = {0xff, 0xff, 0xff, 0xff};
                List<Integer> four8s = Arrays.asList(four);
                @SuppressWarnings("unchecked")
                CellLoop<UpDown>[] nozzles = new CellLoop[3];
                for (int i = 0; i < 3; i++)
                    nozzles[i] = new CellLoop<UpDown>();

                Cell<Double> calibration = new Cell<>(0.001);
                Cell<Double> price1 = textPrice1.text.map(parseDbl);
                Cell<Double> price2 = textPrice2.text.map(parseDbl);
                Cell<Double> price3 = textPrice3.text.map(parseDbl);
                CellSink<Stream<Unit>> csClearSale = new CellSink<>(new Stream<Unit>());
                Stream<Unit> sClearSale = Cell.switchS(csClearSale);

                Cell<Outputs> outputs = logic.selectedItem.map(
                    pump -> pump.create(
                        new Inputs(
                            Operational.updates(nozzles[0]),
                            Operational.updates(nozzles[1]),
                            Operational.updates(nozzles[2]),
                            sKey,
                            sFuelPulses,
                            calibration,
                            price1,
                            price2,
                            price3,
                            sClearSale
                        )
                    )
                );

                delivery = Cell.switchC(outputs.map(o -> o.delivery));
                Cell<String> presetLCD = Cell.switchC(outputs.map(o -> o.presetLCD));
                Cell<String> saleCostLCD = Cell.switchC(outputs.map(o -> o.saleCostLCD));
                Cell<String> saleQuantityLCD = Cell.switchC(outputs.map(o -> o.saleQuantityLCD));
                Cell<String> priceLCD1 = Cell.switchC(outputs.map(o -> o.priceLCD1));
                Cell<String> priceLCD2 = Cell.switchC(outputs.map(o -> o.priceLCD2));
                Cell<String> priceLCD3 = Cell.switchC(outputs.map(o -> o.priceLCD3));
                Stream<Unit> sBeep = Cell.switchS(outputs.map(o -> o.sBeep));
                Stream<Sale> sSaleComplete = Cell.switchS(outputs.map(o -> o.sSaleComplete));

                AudioClip beepClip = Applet.newAudioClip(new URL(rootURL, "../sounds/beep.wav"));
                l = l.append(sBeep.listen(u -> {
                    System.out.println("BEEP!");
                    beepClip.play();
                }));

                AudioClip fastRumble = Applet.newAudioClip(new URL(rootURL, "../sounds/fast.wav"));
                AudioClip slowRumble = Applet.newAudioClip(new URL(rootURL, "../sounds/slow.wav"));

                l = l.append(changes(delivery).listen(d -> {
                    switch (d) {
                    case FAST1:
                    case FAST2:
                    case FAST3:
                        fastRumble.loop();
                        break;
                    default:
                        fastRumble.stop();
                    }
                    switch (d) {
                    case SLOW1:
                    case SLOW2:
                    case SLOW3:
                        slowRumble.loop();
                        break;
                    default:
                        slowRumble.stop();
                    }
                }));

                PumpFace face = new PumpFace(
                        rootURL, sClick,
                        format7Seg(presetLCD,5),
                        format7Seg(saleCostLCD,5),
                        format7Seg(saleQuantityLCD,5),
                        format7Seg(priceLCD1,4),
                        format7Seg(priceLCD2,4),
                        format7Seg(priceLCD3,4),
                        nozzles[0],
                        nozzles[1],
                        nozzles[2]
                    );
                add(face, BorderLayout.CENTER);
                for (int i = 0; i < 3; i++) {
                    final Cell<Tuple2<Rectangle, UpDown>> rect_state =
                        face.nozzleRects[i].lift(nozzles[i],
                            (rect, state) -> new Tuple2<Rectangle, UpDown>(rect, state));
                    ((CellLoop<UpDown>)nozzles[i]).loop(
                        Stream.<UpDown>filterOptional(
                            sClick.snapshot(rect_state,
                                (pt, rs) -> rs.a.contains(pt) ? Optional.of(invert(rs.b))
                                                              : Optional.empty()
                            )
                        ).hold(UpDown.DOWN)
                    );
                }

                l = l.append(sSaleComplete.listen(sale -> {
                    SwingUtilities.invokeLater(() -> {
                        JDialog dialog = new JDialog(this, "Sale complete", false);
                        dialog.setLayout(new GridLayout(5,2));
                        dialog.add(new JLabel("Fuel "));
                        dialog.add(new JLabel(sale.fuel.toString()));
                        dialog.add(new JLabel("Price "));
                        dialog.add(new JLabel(Formatters.priceFmt.format(sale.price)));
                        dialog.add(new JLabel("Dollars delivered "));
                        dialog.add(new JLabel(Formatters.costFmt.format(sale.cost)));
                        dialog.add(new JLabel("Liters delivered "));
                        dialog.add(new JLabel(Formatters.quantityFmt.format(sale.quantity)));
                        SButton ok = new SButton("OK");
                        dialog.add(ok);
                        dialog.pack();
                        dialog.setVisible(true);
                        csClearSale.send(ok.sClicked);
                        this.l = l.append(ok.sClicked.listen(u -> {
                            dialog.dispose();
                        }));
                    });
                }));
            }
            catch (MalformedURLException e) {
                System.err.println("Unexpected exception: "+e);
            }
            catch (IOException e) {
                System.err.println("Unexpected exception: "+e);
            }
        });
        pack();
    }

    private static UpDown invert(UpDown u) {
        return u == UpDown.UP ? UpDown.DOWN : UpDown.UP;
    }

    public static Stream<Key> toKey(Stream<Point> sClick) {
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

        return Stream.filterOptional(sClick.map(pt -> {
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

        // Simuate fuel pulses when 'delivery' is on.
        while (true) {
            Transaction.runVoid(() -> {
                switch (view.delivery.sample()) {
                case FAST1:
                case FAST2:
                case FAST3:
                    view.sFuelPulses.send(40);
                    break;
                case SLOW1:
                case SLOW2:
                case SLOW3:
                    view.sFuelPulses.send(2);
                }
            });
            try { Thread.sleep(200); } catch (InterruptedException e) {}
        }
    }
}

