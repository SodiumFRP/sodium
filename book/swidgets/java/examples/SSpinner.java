import javax.swing.*;
import java.awt.*;
import swidgets.*;
import nz.sodium.*;

public class SSpinner extends JPanel {
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
        add(textField, c);
        c.gridwidth = 1;
        c.gridheight = 1;
        c.gridx = 1;
        c.gridy = 0;
        add(plus, c);
        c.gridx = 1;
        c.gridy = 1;
        add(minus, c);

        Stream<Integer> sPlusDelta = plus.sClicked.map(u -> 1);
        Stream<Integer> sMinusDelta = minus.sClicked.map(u -> -1);
        Stream<Integer> sDelta = sPlusDelta.orElse(sMinusDelta);
        sSetValue.loop(
            sDelta.snapshot(
                this.value,
                (delta, value) -> delta + value
            ));
    }

    public final Cell<Integer> value;
}

