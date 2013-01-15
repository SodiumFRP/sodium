package org.reactiveprogramming.takahemail;

import javax.mail.*;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.Properties;
import fj.F;
import fj.Unit;

public class IMAPConversation
{
    private Thread t;

    public IMAPConversation(IMAPSettings settings)
    {
        this.settings = settings;
        t = new Thread(() -> {
            while (true) {
                try {
                    System.out.println("connecting to IMAP server at "+settings.user+"@"+settings.host+":"+settings.port);
                    Properties props = new Properties();
                    if (settings.tls) {
                        props.put("mail.imaps.ssl.checkserveridentity", "false");
                        props.put("mail.imaps.ssl.trust", "*");
                    }
                    Session session = Session.getDefaultInstance(props, null);

                    Store store = session.getStore(settings.tls ? "imaps" : "imap");
                    store.connect(settings.host, settings.port, settings.user, settings.password);
                    try {
                        while (true) {
                            F<Store, Unit> job = jobQ.take();
                            job.f(store);
                        }
                    }
                    finally {
                        store.close();
                    }
                }
                catch (MessagingException e) {
                    System.out.println("IMAPConversation failed: "+e.toString());
                }
                catch (InterruptedException e) {
                    System.out.println("IMAPConversation failed: "+e.toString());
                }
                try {
                    Thread.sleep(10000);
                }
                catch (InterruptedException e) {
                }
            }
        });
        t.start();
    }

    private IMAPSettings settings;
    private LinkedBlockingQueue<F<Store,Unit>> jobQ = new LinkedBlockingQueue<F<Store,Unit>>();

    public void schedule(final F<Store, Unit> f)
    {
        try {
            jobQ.put(f);
        }
        catch (InterruptedException e) {
            throw new RuntimeException(e.toString());
        }
    }
}

