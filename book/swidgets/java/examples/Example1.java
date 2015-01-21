import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import sodium.*;

public class Example1 {
    public static void main(String[] args) {
        JFrame view = new JFrame("Example1") {
            public Dimension getPreferredSize() {
                return new Dimension(400, 160);
            }
        };
        Transaction.runVoid(() -> {
            view.setLayout(new FlowLayout());

            SSpinner spnr = new SSpinner();
            view.add(spnr);

            view.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent e) {
                    System.exit(0);
                }
            });
            view.pack();
        });
        view.setVisible(true);
    }
}

