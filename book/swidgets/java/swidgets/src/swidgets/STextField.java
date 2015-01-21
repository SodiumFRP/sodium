package swidgets;

import sodium.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.*;
import javax.swing.SwingUtilities;
import java.util.concurrent.ArrayBlockingQueue;

public class STextField extends JTextField
{
    public STextField(String initText, Stream<String> sText, int width)
    {
        super(initText, width);

        allow = sText.map(u -> 1).merge(sDecrement).accum(0, (d, b) -> b + d).map(b -> b == 0);

        final StreamSink<String> sUserText = new StreamSink<String>();
        this.text = sUserText.gate(allow).merge(sText).hold(initText);
        DocumentListener dl = new DocumentListener() {
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
                sUserText.send(getText());
            }
        };

        getDocument().addDocumentListener(dl);

        l = sText.listen(text -> {
            SwingUtilities.invokeLater(() -> {
                setText(text);
                sDecrement.send(-1);
            });
        });
    }

    private final StreamSink<Integer> sDecrement = new StreamSink<>();
    private final Cell<Boolean> allow;
    private final Listener l;
    public final Cell<String> text;

    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }
}

