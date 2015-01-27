package swidgets;

import sodium.*;
import javax.swing.*;
import javax.swing.SwingUtilities;
import java.util.concurrent.ArrayBlockingQueue;

public class SLabel extends JLabel
{
    public SLabel(Cell<String> text) {
        super(text.sample());
        l = text.updates().listen(t -> {
            if (SwingUtilities.isEventDispatchThread())
                setText(t);
            else
                SwingUtilities.invokeLater(() -> {
                    setText(t);
                });
        });
    }

    private final Listener l;

    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }
}

