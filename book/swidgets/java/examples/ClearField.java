import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import swidgets.*;
import sodium.*;

public class ClearField {
    public static void main(String[] args) {
        JFrame view = new JFrame("ClearField");
        view.setLayout(new FlowLayout());

        SButton clear = new SButton("Clear");
        Stream<String> sClearIt = clear.sClicked.map(u -> "");
        STextField text = new STextField(sClearIt, "Hello", 15);
        view.add(text);
        view.add(clear);

        view.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        view.setSize(400, 160);
        view.setVisible(true);
    }
}

