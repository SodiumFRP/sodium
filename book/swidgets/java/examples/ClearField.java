import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import swidgets.*;
import sodium.*;

public class ClearField {
    public static void main(String[] args) {
        JFrame view = new JFrame("ClearField") {
            public Dimension getPreferredSize() {
                return new Dimension(400, 160);
            }
        };
        view.setLayout(new FlowLayout());

        Transaction.runVoid(() -> {
            SButton clear = new SButton("Clear");
            Stream<String> sClearIt = clear.sClicked.map(u -> "");
            STextField text = new STextField(sClearIt, "Hello", 15);
            view.add(text);
            view.add(clear);
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

