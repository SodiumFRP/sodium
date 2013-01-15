package org.reactiveprogramming.takahemail;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.plaf.LayerUI;
import javax.mail.Folder;
import fj.data.List;
import fj.data.Option;
import java.util.LinkedList;

public class FolderView extends JList<String>
{
    private static final String[] data = {"one", "two", "three", "four"};

    private Option<List<Folder>> oFolders = Option.none();

    private static class WaitLayerUI extends LayerUI<JList<String>>
    {
        WaitLayerUI(FolderView v) { this.v = v; }
        private FolderView v;

    }

    private LinkedList<ListDataListener> listeners = new LinkedList<ListDataListener>();

    private void setFolders(ListModel<String> lm, Option<List<Folder>> oFolders)
    {
        this.oFolders = oFolders;
        ListDataEvent e = new ListDataEvent(lm, ListDataEvent.CONTENTS_CHANGED, 0,
            oFolders.isSome() ? oFolders.some().length() : 0);
        for (ListDataListener l : listeners)
            l.contentsChanged(e);
    }

    static JComponent create(IMAPFolders folders)
    {
        final FolderView v = new FolderView();
        final ListModel<String> lm = new ListModel<String>() {
            public String getElementAt(int ix)
            {
                return v.oFolders.some().index(ix).getFullName();
            }
            public int getSize()
            {
                return v.oFolders.isSome() ? v.oFolders.some().length() : 0;
            }
            public void addListDataListener(ListDataListener l)
            {
                v.listeners.add(l);
            }
            public void removeListDataListener(ListDataListener l)
            {
                v.listeners.remove(l);
            }
        };
        JList<String> l = new JList(lm);
        final LayerUI<JList<String>> layerUI = new LayerUI<JList<String>>() {
            @Override
            public void paint(Graphics g, JComponent c)
            {
                int w = c.getWidth();
                int h = c.getHeight();
             
                // Paint the view.
                super.paint (g, c);
    
                if (v.oFolders.isNone()) {
                    // Grey it out
                    Graphics2D g2 = (Graphics2D)g.create();
                    Composite urComposite = g2.getComposite();
                    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, .5f));
                    g2.fillRect(0, 0, w, h);
                    g2.setComposite(urComposite);
                }
            }
        };
        JLayer layer = new JLayer<JList<String>>(l, layerUI);
        folders.addListener(oFolders -> {
            SwingUtilities.invokeLater(() -> {
                v.setFolders(lm, oFolders);
                layer.repaint();
            });
        });
        return layer;
    }
}
