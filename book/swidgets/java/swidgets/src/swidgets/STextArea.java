package swidgets;

import nz.sodium.*;
import java.awt.Point;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.*;
import javax.swing.SwingUtilities;
import java.util.concurrent.ArrayBlockingQueue;

public class STextArea extends JTextArea
{
    public STextArea(String initText, int rows, int columns) {
        this(new Stream<String>(), initText, rows, columns);
    }

    public STextArea(String initText) {
        this(new Stream<String>(), initText);
    }

    public STextArea(Stream<String> sText, String initText, int rows, int columns) {
        this(sText, initText, rows, columns, new Cell<Boolean>(true));
    }

    public STextArea(Stream<String> sText, String initText) {
        this(sText, initText, new Cell<Boolean>(true));
    }

    public STextArea(String initText, int rows, int columns, Cell<Boolean> enabled) {
        this(new Stream<String>(), initText, rows, columns, enabled);
    }

    public STextArea(String initText, Cell<Boolean> enabled) {
        this(new Stream<String>(), initText, enabled);
    }

    public STextArea(Stream<String> sText, String initText, Cell<Boolean> enabled) {
        super(initText);
        setup(sText, initText, enabled);
    }

    public STextArea(Stream<String> sText, String initText, int rows, int columns, Cell<Boolean> enabled) {
        super(initText, rows, columns);
        setup(sText, initText, enabled);
    }

    /**
     * Non-editable text area with text defined by a cell.
     */
    public STextArea(Cell<String> text) {
        this(Operational.updates(text), text.sample());
        setEditable(false);
    }

    /**
     * Non-editable text area with text defined by a cell.
     */
    public STextArea(Cell<String> text, int rows, int columns) {
        this(Operational.updates(text), text.sample(), rows, columns);
        setEditable(false);
    }

    /**
     * Non-editable text area with text defined by a cell.
     */
    public STextArea(Cell<String> text, Cell<Boolean> enabled) {
        this(Operational.updates(text), text.sample(), enabled);
        setEditable(false);
    }

    /**
     * Non-editable text area with text defined by a cell.
     */
    public STextArea(Cell<String> text, int rows, int columns, Cell<Boolean> enabled) {
        this(Operational.updates(text), text.sample(), rows, columns, enabled);
        setEditable(false);
    }

    private void setup(Stream<String> sText, String initText, Cell<Boolean> enabled) {
        allow = sText.map(u -> 1).orElse(sDecrement).accum(0, (d, b) -> b + d).map(b -> b == 0);

        final StreamSink<String> sUserText = new StreamSink<String>();
        this.text = sUserText.gate(allow).orElse(sText).hold(initText);
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

        // Do it at the end of the transaction so it works with looped cells
        Transaction.post(() -> setEnabled(enabled.sample()));
        l = sText.listen(text -> {
            SwingUtilities.invokeLater(() -> {
                setText(text);
                sDecrement.send(-1);
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

    private StreamSink<Integer> sDecrement = new StreamSink<>();
    private Cell<Boolean> allow;
    private Listener l;
    public Cell<String> text;

    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }
}

