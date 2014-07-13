import sodium.*;
import java.awt.event.*;
import javax.swing.JButton;

class SButton extends JButton
{
    public SButton(String text)
    {
        super(text);
        EventSink<Unit> eClicked = new EventSink<Unit>();
        addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                eClicked.send(Unit.UNIT);
            }
        });
        this.eClicked = eClicked;
    }

    public final Event<Unit> eClicked;
}
