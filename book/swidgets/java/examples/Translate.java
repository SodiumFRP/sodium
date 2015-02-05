import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import swidgets.*;
import sodium.*;

public class Translate {
    public static void main(String[] args) {
        JFrame view = new JFrame("Translate");
        view.setLayout(new FlowLayout());

        Transaction.runVoid(() -> {
            StreamLoop<String> sTranslate = new StreamLoop<>();
            STextField text = new STextField(sTranslate,
                "I like FRP", 15);
            SButton translate = new SButton("Translate");
            sTranslate.loop(
                translate.sClicked.snapshot(text.text, (u, txt) ->
                    txt.trim().replaceAll(" |$", "us ").trim()
                )
            );
            view.add(text);
            view.add(translate);
        });

        view.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        view.setSize(400, 160);
        view.setVisible(true);
    }
}

