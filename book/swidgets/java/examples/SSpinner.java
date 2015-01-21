import javax.swing.*;
import java.awt.*;
import swidgets.*;
import sodium.*;

public class SSpinner extends JComponent
{
    SSpinner()
    {
        StreamLoop<String> sSetText = new StreamLoop<>();
        STextField textField = new STextField("0", sSetText, 5);
        this.text = textField.text;
        SButton up = new SButton("+");
        SButton down = new SButton("-");

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
        gridbag.setConstraints(up, c);
        add(up);
        c.gridx = 1;
        c.gridy = 1;
        gridbag.setConstraints(down, c);
        add(down);

        Stream<Integer> sUpDelta = up.sClicked.map(u -> 1);
        Stream<Integer> sDownDelta = down.sClicked.map(u -> -1);
        Stream<Integer> sDelta = sUpDelta.merge(sDownDelta);
        sSetText.loop(
            sDelta.snapshot(
                textField.text,
                (delta, txt) -> {
                    try {
                        return Integer.toString(Integer.parseInt(txt) + delta);
                    }
                    catch (NumberFormatException e) {
                        System.err.println("can't parse "+txt);
                        return txt;
                    }
                }
            )
        );
    }

    public final Cell<String> text;
}

