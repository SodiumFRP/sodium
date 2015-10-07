import fridgets.*;
import javax.swing.*;
import java.util.ArrayList;
import nz.sodium.*;

public class textfield {
    public static void main(String[] args) {
        JFrame frame = new JFrame("button");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setContentPane(Transaction.run(() -> {
            FrTextField firstName = new FrTextField("Joe");
            FrTextField lastName = new FrTextField("Bloggs");
            FrButton ok = new FrButton(new Cell<>("OK"));
            FrButton cancel = new FrButton(new Cell<>("Cancel"));
            ArrayList<Fridget> fridgets = new ArrayList<>();
            fridgets.add(ok);
            fridgets.add(cancel);
            Fridget buttons = new FrFlow(FrFlow.Direction.HORIZONTAL,
                fridgets);
            fridgets = new ArrayList<>();
            fridgets.add(firstName);
            fridgets.add(lastName);
            fridgets.add(buttons);
            Fridget dialog =
                new FrFlow(FrFlow.Direction.VERTICAL, fridgets);
            Listener l =
                ok.sClicked
                      .map(u -> firstName.text.sample()+" "+
                                lastName.text.sample())
                      .listen(name -> System.out.println("OK: "+name))
                  .append(
                      cancel.sClicked.listen(
                          u -> System.out.println("Cancel")
                      )
                  );
            return new FrView(frame, dialog) {
                public void removeNotify() {
                    super.removeNotify();
                    l.unlisten();
                }
            };
        }));
        frame.setSize(360,120);
        frame.setVisible(true);
    }
}

