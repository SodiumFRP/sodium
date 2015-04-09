import fridgets.*;
import javax.swing.*;
import sodium.*;

public class button {
    public static void main(String[] args) {
        JFrame frame = new JFrame("button");
        FrButton b = new FrButton(new Cell<>("OK"));
        View<Stream<Unit>> view = new View<>(b);
        view.out.listen(u -> System.out.println("clicked!"));
        frame.setContentPane(view);
        frame.pack();
        frame.setVisible(true);
    }
}

