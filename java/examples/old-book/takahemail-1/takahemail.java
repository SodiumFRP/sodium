import org.reactiveprogramming.takahemail.*;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class takahemail
{
    public static void main(String[] args)
    {
        String host = args[0];
        int port = Integer.parseInt(args[1]);
        boolean tls = args[2].equals("true");
        String user = args[3];
        String password = args[4];
        IMAPSettings settings = new IMAPSettings(host, port, tls, user, password);
        IMAPConversation conv = new IMAPConversation(settings);
        IMAPFolders folders = new IMAPFolders(conv);
        AccountView view = new AccountView("Takahe Mail", folders);
        view.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        view.setSize(700, 500);
        //view.pack();
        view.setVisible(true);
    }
}

