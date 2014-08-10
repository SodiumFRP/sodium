import javax.swing.JFrame;
import java.awt.*;
import java.awt.event.*;
import sodium.*;

public class Listing_2_1 extends JFrame
{
    public Listing_2_1() {
        setPreferredSize(new Dimension(300,120));
        setLayout(new FlowLayout(FlowLayout.CENTER));
        SButton ok = new SButton("OK");
        add(ok);

        Listener l = ok.eClicked.listen((Unit u) -> {
            System.out.println("OK");
        });
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                l.unlisten();
            }
        });
    }

    public static void main(String[] args) {
        Listing_2_1 frame = new Listing_2_1();
        frame.pack();
        frame.setVisible(true);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }
}
