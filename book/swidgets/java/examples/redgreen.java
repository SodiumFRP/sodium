import javax.swing.*;
import java.awt.FlowLayout;
import swidgets.*;
import nz.sodium.*;

public class redgreen {
    public static void main(String[] args) {
        JFrame frame = new JFrame("redgreen");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLayout(new FlowLayout());
        SButton red = new SButton("red");
        SButton green = new SButton("green");
        Stream<String> sRed = red.sClicked.map(u -> "red");
        Stream<String> sGreen = green.sClicked.map(u -> "green");
        Stream<String> sColor = sRed.orElse(sGreen);
        Cell<String> color = sColor.hold("");
        SLabel lbl = new SLabel(color);
        frame.add(red);
        frame.add(green);
        frame.add(lbl);
        frame.setSize(400, 160);
        frame.setVisible(true);
    }
}

