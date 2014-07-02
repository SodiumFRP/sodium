package org.reactiveprogramming.takahemail;

import fj.data.Option;
import fj.data.List;
import fj.Unit;
import javax.mail.Folder;
import javax.mail.MessagingException;
import javax.mail.Store;
import java.util.LinkedList;
import java.util.Arrays;

public class IMAPFolders
{
    private IMAPConversation conv;
    private Option<List<Folder>> folders;

    private static List<Folder> recursively(Folder f)
        throws MessagingException
    {
        List<Folder> out = List.<Folder>cons(f, List.<Folder>nil());
        for (Folder subf : List.<Folder>iterableList(Arrays.asList(f.list())))
            out = out.append(recursively(subf));
        return out;
    }

    public IMAPFolders(IMAPConversation conv)
    {
        this.conv = conv;
        conv.schedule(store -> {
            Option<List<Folder>> oFolders = Option.<List<Folder>>none();
            try {
                List<Folder> folders = recursively(store.getDefaultFolder());
                oFolders = Option.<List<Folder>>some(folders);
            }
            catch (MessagingException me) {
                System.out.println("IMAPFolders: "+me);
            }
            // Nice if this was atomic
            synchronized (IMAPFolders.this) { IMAPFolders.this.folders = oFolders; }
            notifyFoldersUpdated(folders);
            return Unit.unit();
        });
    }

    public interface Listener {
        public void foldersUpdated(Option<List<Folder>> oFolders);
    }
    private LinkedList<Listener> listeners = new LinkedList<Listener>();

    public synchronized Option<List<Folder>> getFolders()
    {
        return folders;
    }

    public synchronized void addListener(Listener l)
    {
        listeners.add(l);
    }

    public synchronized void removeListener(Listener l)
    {
        listeners.remove(l);
    }

    private void notifyFoldersUpdated(Option<List<Folder>> folders)
    {
        LinkedList<Listener> listeners;
        synchronized (this) { listeners = (LinkedList<Listener>)this.listeners.clone(); }
        for (Listener l : listeners)
            l.foldersUpdated(folders);
    }
}

