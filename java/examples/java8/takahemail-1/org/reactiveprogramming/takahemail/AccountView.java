package org.reactiveprogramming.takahemail;

import javax.swing.*;

public class AccountView extends JFrame
{
    public AccountView(String title, IMAPFolders folders)
    {
        super(title);
        this.folders = folders;

        JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
            new JScrollPane(FolderView.create(folders)), FolderView.create(folders));
        split.setDividerLocation(200);
        add(split);
    }

    private IMAPFolders folders;
}

