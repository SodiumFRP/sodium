import javax.swing.*;
import java.awt.*;
import java.lang.reflect.Array;
import swidgets.*;
import nz.sodium.*;

public class formvalidation {
    public static void main(String[] args) {
        JFrame view = new JFrame("formvalidation");
        view.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        Transaction.runVoid(() -> {
            final int maxEmails = 4;

            JLabel[] labels = new JLabel[maxEmails+2];
            JComponent[] fields = new JComponent[maxEmails+2];
            Cell<String>[] valids = (Cell<String>[])Array.newInstance(
                Cell.class, maxEmails+2);
            int row = 0;

            labels[row] = new JLabel("Name");
            STextField name = new STextField("", 30);
            fields[row] = name;
            valids[row] = name.text.map(t ->
                t.trim().equals("")       ? "<-- enter something" :
                t.trim().indexOf(' ') < 0 ? "<-- must contain space" :
                                            "");
            row++;

            labels[row] = new JLabel("No of email addresses");
            SSpinner number = new SSpinner(1);
            fields[row] = number;
            valids[row] = number.value.map(n ->
                n < 1 || n > maxEmails ? "<-- must be 1 to "+maxEmails
                                       : "");
            row++;

            STextField[] emails = new STextField[maxEmails]; 
            for (int i = 0; i < maxEmails; i++, row++) {
                labels[row] = new JLabel("Email #"+(i+1));
                final int ii = i;
                Cell<Boolean> enabled = number.value.map(n -> ii < n);
                STextField email = new STextField("", 30, enabled);
                fields[row] = email;
                valids[row] = email.text.lift(number.value, (e, n) ->
                    ii >= n             ? "" :
                    e.trim().equals("") ? "<-- enter something" :
                    e.indexOf('@') < 0  ? "<-- must contain @" :
                                          "");
            }

            GridBagLayout gridbag = new GridBagLayout();
            view.setLayout(gridbag);
            GridBagConstraints c = new GridBagConstraints();
            c.fill = GridBagConstraints.BOTH;
            c.gridwidth = 1;
            c.gridheight = 1;
            Cell<Boolean> allValid = new Cell<Boolean>(true);
            for (int i = 0; i < row; i++) {
                c.weightx = 0;
                c.gridx = 0;
                c.gridy = i;
                view.add(labels[i], c);
                c.weightx = 1.0;
                c.gridx = 1;
                view.add(fields[i], c);
                c.weightx = 0;
                c.gridx = 2;
                SLabel validLabel = new SLabel(valids[i]);
                view.add(validLabel, c);
                Cell<Boolean> thisValid = valids[i].map(t -> t.equals(""));
                allValid = allValid.lift(thisValid, (a, b) -> a && b);
            }
            c.weightx = 1.0;
            c.gridx = 0;
            c.gridy = row;
            c.gridwidth = 3;
            c.fill = GridBagConstraints.NONE;
            SButton ok = new SButton("OK", allValid);
            view.add(ok, c);
        });
        view.setSize(600, 200);
        view.setVisible(true);
    }
}

