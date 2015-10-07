package swidgets;

import nz.sodium.*;
import javax.swing.JButton;
import javax.swing.SwingUtilities;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class SButton extends JButton {
    public SButton(String label) {
        this(label, new Cell<Boolean>(true));
    }

    public SButton(String label, Cell<Boolean> enabled) {
        super(label);
        StreamSink<Unit> sClickedSink = new StreamSink<>();
        this.sClicked = sClickedSink;
        addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                sClickedSink.send(Unit.UNIT);
            }
        });
        // Do it at the end of the transaction so it works with looped cells
        Transaction.post(() -> setEnabled(enabled.sample()));
        l = Operational.updates(enabled).listen(
            ena -> {
                if (SwingUtilities.isEventDispatchThread())
                    this.setEnabled(ena);
                else {
                    SwingUtilities.invokeLater(() -> {
                        this.setEnabled(ena);
                    });
                }
            }
        );
    }

    private final Listener l;
    public final Stream<Unit> sClicked;

    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }
}
