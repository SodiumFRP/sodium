package pump;

import sodium.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.*;

public class STextField extends JTextField
{
    public STextField(String initText, int width)
    {
        super(initText, width);
        BehaviorSink<String> text = new BehaviorSink<String>(initText);
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

    public final Behavior<String> text;
}

