import javax.swing.*;
import java.awt.*;
import swidgets.*;
import sodium.*;

public class SSpinner extends JComponent {
    SSpinner(int initialValue) {
        StreamLoop<Integer> sSetValue = new StreamLoop<>();
        STextField textField = new STextField(
            sSetValue.map(v -> Integer.toString(v)),
            Integer.toString(initialValue),
            5
        );
        this.value = textField.text.map(txt -> {
            try {
                return Integer.parseInt(txt);
            }
            catch (NumberFormatException e) {
                return 0;
            }
        });
        SButton plus = new SButton("+");
        SButton minus = new SButton("-");

        GridBagLayout gridbag = new GridBagLayout();
        setLayout(gridbag);
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridx = 0;
        c.gridy = 0;
        c.gridwidth = 1;
        c.gridheight = 2;
        gridbag.setConstraints(textField, c);
        add(textField);
        c.gridwidth = 1;
        c.gridheight = 1;
        c.gridx = 1;
        c.gridy = 0;
        gridbag.setConstraints(plus, c);
        add(plus);
        c.gridx = 1;
        c.gridy = 1;
        gridbag.setConstraints(minus, c);
        add(minus);

        Stream<Integer> sPlusDelta = plus.sClicked.map(u -> 1);
        Stream<Integer> sMinusDelta = minus.sClicked.map(u -> -1);
        Stream<Integer> sDelta = sPlusDelta.merge(sMinusDelta);
        sSetValue.loop(
            sDelta.snapshot(
                this.value,
                (delta, value) -> delta + value
            ));
    }

    public final Cell<Integer> value;
}

