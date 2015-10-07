package swidgets;

import nz.sodium.*;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import javax.swing.*;
import java.util.Optional;
import java.util.Vector;

public class SComboBox<E> extends JComboBox
{
    public SComboBox() {
        selectedItem = mkSelectedItem(this);
    }
    public SComboBox(ComboBoxModel<E> aModel) {
        super(aModel);
        selectedItem = mkSelectedItem(this);
    }
    public SComboBox(E[] items) {
        super(items);
        selectedItem = mkSelectedItem(this);
    }
    public SComboBox(Vector<E> items) {
        super(items);
        selectedItem = mkSelectedItem(this);
    }

    @SuppressWarnings("unchecked")
    private static <E> Cell<Optional<E>> mkSelectedItem(JComboBox box) {
        E sel = (E)box.getSelectedItem();
        CellSink<Optional<E>> selectedItem = new CellSink<Optional<E>>(
            sel == null ? Optional.<E>empty() : Optional.of(sel));
        box.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED)
                    selectedItem.send(Optional.of((E)e.getItem()));
            }
        });
        return selectedItem;
    }

    public final Cell<Optional<E>> selectedItem;
}

