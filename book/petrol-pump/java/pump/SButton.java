package pump;

import nz.sodium.*;
import javax.swing.JButton;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class SButton extends JButton
{
    public SButton(String label)
    {
        super(label);
        StreamSink<Unit> sClickedSink = new StreamSink<>();
        this.sClicked = sClickedSink;
        addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                sClickedSink.send(Unit.UNIT);
            }
        });
    }

    public final Stream<Unit> sClicked;
}
