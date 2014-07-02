package org.reactiveprogramming.takahemail;

public class IMAPSettings
{
    public IMAPSettings(
        String host,
        int port,
        boolean tls,
        String user,
        String password)
    {
        this.host = host;
        this.port = port;
        this.tls = tls;
        this.user = user;
        this.password = password;
    }
    
    String host;
    int port;
    boolean tls;
    String user;
    String password;
}

