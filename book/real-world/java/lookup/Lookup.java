package lookup;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import swidgets.*;
import nz.sodium.*;
import java.util.Optional;

class IsBusy<A,B> {
    public IsBusy(Lambda1<Stream<A>, Stream<B>> action, Stream<A> sIn) {
        sOut = action.apply(sIn);
        busy = sOut.map(i -> false)
                   .orElse(sIn.map(i -> true))
                   .hold(false);
    }
    public final Stream<B> sOut;
    public final Cell<Boolean> busy;
}

public class Lookup {

    public static final
    Lambda1<Stream<String>, Stream<Optional<String>>> lookup = sWord -> {
        StreamSink<Optional<String>> sDefinition = new StreamSink<>();
        Listener l = sWord.listenWeak(wrd -> {
            new Thread() {
                public void run() {
                    System.out.println("look up "+wrd);
                    Optional<String> def = Optional.empty();
                    try {
                        Socket s = new Socket(InetAddress.getByName(
                            "dict.org"), 2628);
                        try {
                            BufferedReader r = new BufferedReader(
                                new InputStreamReader(s.getInputStream(),
                                    "UTF-8"));
                            PrintWriter w = new PrintWriter(
                                new OutputStreamWriter(s.getOutputStream(),
                                    "UTF-8"));
                            String greeting = r.readLine();
                            w.println("DEFINE ! "+wrd);
                            w.flush();
                            String result = r.readLine();
                            if (result.startsWith("150"))
                                result = r.readLine();
                            if (result.startsWith("151")) {
                                StringBuffer b = new StringBuffer();
                                while (true) {
                                    String l = r.readLine();
                                    if (l.equals("."))
                                        break;
                                    b.append(l+"\n");
                                }
                                def = Optional.of(b.toString());
                            }
                            else
                                System.out.println("ERROR: "+result);
                        }
                        finally {
                            try { s.close(); } catch (IOException e) {}
                        }
                    }
                    catch (UnknownHostException e) {
                        System.out.println(e.toString());
                    }
                    catch (IOException e) {
                        System.out.println(e.toString());
                    }
                    finally {
                        sDefinition.send(def);
                    }
                }
            }.start();
        });
        return sDefinition.addCleanup(l);
    };

    public static void main(String[] args) {
        JFrame view = new JFrame("Dictionary lookup");
        GridBagLayout gridbag = new GridBagLayout();
        view.setLayout(gridbag);
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weighty = 0.0;
        c.weightx = 1.0;
        c.gridwidth = 1;
        c.gridheight = 1;
        c.gridx = 0;
        c.gridy = 0;

        Transaction.runVoid(() -> {
            STextField word = new STextField("", 25);
            CellLoop<Boolean> enabled = new CellLoop<>();
            SButton button = new SButton("look up", enabled);
            Stream<String> sWord = button.sClicked.snapshot(word.text);
            IsBusy<String, Optional<String>> ib =
                                     new IsBusy<>(lookup, sWord);
            Stream<String> sDefinition = ib.sOut
                .map(o -> o.orElse("ERROR!"));
            Cell<String> definition = sDefinition.hold("");
            Cell<String> output = definition.lift(ib.busy, (def, bsy) ->
                bsy ? "Looking up..." : def);
            enabled.loop(ib.busy.map(b -> !b));
            STextArea outputArea = new STextArea(output, enabled);
            view.add(word, c);
            c.gridx = 1;
            view.add(button, c);
            c.fill = GridBagConstraints.BOTH;
            c.weighty = 1.0;
            c.gridwidth = 2;
            c.gridx = 0;
            c.gridy = 1;
            view.add(new JScrollPane(outputArea), c);
        });

        view.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        view.setSize(500,250);
        view.setVisible(true);
    }
}

