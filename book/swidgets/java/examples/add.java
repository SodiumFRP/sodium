import javax.swing.*;
import java.awt.FlowLayout;
import swidgets.*;
import nz.sodium.*;

public class add {
    public static void main(String[] args) {
        JFrame frame = new JFrame("add");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLayout(new FlowLayout());
        STextField txtA = new STextField("5");
        STextField txtB = new STextField("10");
        Cell<Integer> a = txtA.text.map(t -> parseInt(t));
        Cell<Integer> b = txtB.text.map(t -> parseInt(t));
        Cell<Integer> sum = a.lift(b, (a_, b_) -> a_ + b_);
        SLabel lblSum = new SLabel(sum.map(i -> Integer.toString(i)));
        frame.add(txtA);
        frame.add(txtB);
        frame.add(lblSum);
        frame.setSize(400, 160);
        frame.setVisible(true);
    }
    private static Integer parseInt(String t) {
        try {
            return Integer.parseInt(t);
        } catch (NumberFormatException e) {
            return 0;
        }
    }
}

