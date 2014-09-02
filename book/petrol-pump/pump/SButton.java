package pump;

import sodium.*;
import javax.swing.JButton;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class SButton extends JButton
{
    public SButton(String label)
    {
        super(label);
        EventSink<Unit> eClickedSink = new EventSink<>();
        this.eClicked = eClickedSink;
        addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                eClickedSink.send(Unit.UNIT);
            }
        });
    }

    public final Event<Unit> eClicked;
}
