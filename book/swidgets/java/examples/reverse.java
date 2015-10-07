import javax.swing.*;
import java.awt.FlowLayout;
import swidgets.*;
import nz.sodium.*;

public class reverse {
    public static void main(String[] args) {
        JFrame frame = new JFrame("reverse");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLayout(new FlowLayout());
        STextField msg = new STextField("Hello");
        Cell<String> reversed = msg.text.map(t ->
            new StringBuilder(t).reverse().toString());
        SLabel lbl = new SLabel(reversed);
        frame.add(msg);
        frame.add(lbl);
        frame.setSize(400, 160);
        frame.setVisible(true);
    }
}

