import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import sodium.*;

public class SpinMe {
    public static void main(String[] args) {
        JFrame view = new JFrame("SpinMe") {
            public Dimension getPreferredSize() {
                return new Dimension(400, 160);
            }
        };
        view.setLayout(new FlowLayout());

        Transaction.runVoid(() -> {
            SSpinner spnr = new SSpinner(0);
            view.add(spnr);
        });

        view.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        view.pack();
        view.setVisible(true);
    }
}

