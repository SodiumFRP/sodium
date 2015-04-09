import fridgets.*;
import javax.swing.*;
import sodium.*;

public class button {
    public static void main(String[] args) {
        JFrame frame = new JFrame("button");
        Listener l = Transaction.run(() -> {
            FrButton b = new FrButton(new Cell<>("OK"));
            frame.setContentPane(new FrView(frame, b));
            return b.sClicked.listen(u -> System.out.println("clicked!"));
        });
        frame.pack();
        frame.setVisible(true);
    }
}

