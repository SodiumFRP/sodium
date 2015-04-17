import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import swidgets.*;
import sodium.*;

public class translate {
    public static void main(String[] args) {
        JFrame view = new JFrame("Translate");
        view.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
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
        view.setSize(400, 160);
        view.setVisible(true);
    }
}

