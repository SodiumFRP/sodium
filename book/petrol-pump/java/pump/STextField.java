package pump;

import nz.sodium.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.*;

public class STextField extends JTextField
{
    public STextField(String initText, int width)
    {
        super(initText, width);
        CellSink<String> text = new CellSink<String>(initText);
        this.text = text;

        getDocument().addDocumentListener(new DocumentListener() {
            public void changedUpdate(DocumentEvent e) {
                update();
            }
            public void removeUpdate(DocumentEvent e) {
                update();
            }
            public void insertUpdate(DocumentEvent e) {
                update();
            }

            public void update() {
                text.send(getText());
            }
        });
    }

    public final Cell<String> text;
}

