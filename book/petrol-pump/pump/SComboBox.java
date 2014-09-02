package pump;

import sodium.*;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import javax.swing.*;

public class SComboBox<A> extends JComboBox
{
    @SuppressWarnings("unchecked")
    public SComboBox(ComboBoxModel<A> aModel)
    {
        super(aModel);

        BehaviorSink<A> selectedItem = new BehaviorSink<A>((A)getSelectedItem());
        addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED)
                    selectedItem.send((A)e.getItem());
            }
        });
        this.selectedItem = selectedItem;
    }

    public final Behavior<A> selectedItem;
}

