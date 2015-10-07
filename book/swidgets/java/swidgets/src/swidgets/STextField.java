package swidgets;

import nz.sodium.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.*;
import java.util.concurrent.ArrayBlockingQueue;

public class STextField extends JTextField
{
    public STextField(String initText) {
        this(new Stream<String>(), initText, 15);
    }
    public STextField(String initText, int width) {
        this(new Stream<String>(), initText, width);
    }
    public STextField(Stream<String> sText, String initText) {
        this(sText, initText, 15);
    }
    public STextField(Stream<String> sText, String initText, int width) {
        this(sText, initText, width, new Cell<Boolean>(true));
    }

    public STextField(String initText, int width, Cell<Boolean> enabled) {
        this(new Stream<String>(), initText, width, enabled);
    }

    public STextField(Stream<String> sText, String initText, int width, Cell<Boolean> enabled) {
        super(initText, width);

        allow = sText.map(u -> 1)  // Block local changes until remote change has
                                   // been completed in the GUI
                     .orElse(sDecrement)
                     .accum(0, (d, b) -> b + d).map(b -> b == 0);

        final StreamSink<String> sUserChangesSnk = new StreamSink<String>();
        this.sUserChanges = sUserChangesSnk;
        this.text = sUserChangesSnk.gate(allow).orElse(sText).hold(initText);
        DocumentListener dl = new DocumentListener() {
            private String text = null;
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
                this.text = getText();
                SwingUtilities.invokeLater(() -> {
                    if (this.text != null) {
                        sUserChangesSnk.send(this.text);
                        this.text = null;
                    }
                });
            }
        };

        getDocument().addDocumentListener(dl);

        // Do it at the end of the transaction so it works with looped cells
        Transaction.post(() -> setEnabled(enabled.sample()));
        l = sText.listen(text -> {
            SwingUtilities.invokeLater(() -> {
                getDocument().removeDocumentListener(dl);
                setText(text);
                getDocument().addDocumentListener(dl);
                sDecrement.send(-1);  // Re-allow blocked remote changes
            });
        }).append(
            Operational.updates(enabled).listen(
                ena -> {
                    if (SwingUtilities.isEventDispatchThread())
                        this.setEnabled(ena);
                    else {
                        SwingUtilities.invokeLater(() -> {
                            this.setEnabled(ena);
                        });
                    }
                }
            )
        );
    }
    private final StreamSink<Integer> sDecrement = new StreamSink<>();
    private final Cell<Boolean> allow;
    private final Listener l;
    public final Cell<String> text;
    public final Stream<String> sUserChanges;

    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }
}

