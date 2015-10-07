import swidgets.*;
import nz.sodium.*;
import java.util.Optional;
import java.awt.FlowLayout;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.*;

class Date {
    public Date(int year, int month, int day) {
        this.year = year; this.month = month; this.day = day; }
    public final int year, month, day;
    public final Date setYear(int year_) {
                                return new Date(year_, month,  day); }
    public final Date setMonth(int month_) {
                                return new Date(year,  month_, day); }
    public final Date setDay(int day_) {
                                return new Date(year,  month,  day_); }
    public String toString() { return year+"."+month+"."+day; }
}

class VTextField extends STextField {
    public VTextField(Value<String> v, int width) {
        this(new StreamLoop<String>(), v, width);
    }
    private VTextField(StreamLoop<String> sRemoteWrite, Value<String> v,
                                                           int width) {
        this(sRemoteWrite, v.construct(sRemoteWrite), width);
    }
    private VTextField(StreamLoop<String> sRemoteWrite,
                   ValueOutput<String> outRemote, int width) {
        super(
            Stream.filterOptional(Operational.value(outRemote.value)),
            "",
            width,
            outRemote.value.map(oV -> oV.isPresent())
        );
        sRemoteWrite.loop(sUserChanges);
        this.cleanup = outRemote.cleanup;
    }
    public void removeNotify() {
        cleanup.unlisten();
        super.removeNotify();
    }
    private Listener cleanup;
}

public class form {
    public static void main(String[] args) {
        JFrame frame = new JFrame("form");
        Listener l = Transaction.run(() -> {
            frame.setLayout(new FlowLayout());
            SButton newClient = new SButton("Open new client");
            frame.add(newClient);
            BackEnd be = new BackEnd();
            Value<String> vName    = be.allocate("name", "Joe Bloggs");
            Value<Date> vBirthDate = be.allocate("birthDate",
                new Date(1980, 5, 1));

            Value<Integer> vYear = vBirthDate.lens(
                d -> d.year,
                (dt, y) -> dt.setYear(y)
            );
            Value<Integer> vMonth = vBirthDate.lens(
                d -> d.month,
                (dt, y) -> dt.setMonth(y)
            );
            Value<Integer> vDay = vBirthDate.lens(
                d -> d.day,
                (dt, y) -> dt.setDay(y)
            );
            Bijection<Integer,String> toString = new Bijection<>(
                    i -> Integer.toString(i),
                    s -> {
                        try { return Integer.parseInt(s); }
                        catch (NumberFormatException e) {
                            return 0;
                        }
                    });
            Value<String> vYearStr = vYear.map(toString);
            Value<String> vMonthStr = vMonth.map(toString);
            Value<String> vDayStr = vDay.map(toString);

            frame.setSize(500,250);
            frame.setVisible(true);
            return newClient.sClicked.listen(u -> {
                SwingUtilities.invokeLater(() -> {
                    JFrame client = new JFrame("form client");
                    GridBagLayout gridbag = new GridBagLayout();
                    client.setLayout(gridbag);
                    GridBagConstraints c = new GridBagConstraints();
                    c.fill = GridBagConstraints.HORIZONTAL;
                    c.weighty = 0.0;
                    c.weightx = 1.0;
                    c.gridwidth = 1;
                    c.gridheight = 1;
                    c.gridx = 0;
                    c.gridy = 0;
                    Transaction.runVoid(() -> {
                        c.gridx = 0;
                        client.add(new JLabel("Name"), c);
                        c.gridx = 1;
                        c.gridwidth = 3;
                        client.add(new VTextField(vName, 15), c);
                        c.gridwidth = 1;

                        c.gridy = 1;
                        c.gridx = 0;
                        client.add(new JLabel("Birth date"), c);
                        c.gridx = 1;
                        client.add(new VTextField(vYearStr, 4), c);
                        c.gridx = 2;
                        client.add(new VTextField(vMonthStr, 2), c);
                        c.gridx = 3;
                        client.add(new VTextField(vDayStr, 2), c);

                        client.setSize(300,100);
                        client.setVisible(true);
                    });
                    client.addWindowListener(new WindowAdapter() {
                        public void windowClosing(WindowEvent e) {
                            client.dispose();
                        }
                    });
                });
            });
        });
        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                l.unlisten();
                System.exit(0);
            }
        });
        while (true) {
            Runtime.getRuntime().gc();
            try { Thread.sleep(1000); } catch (InterruptedException e) {}
        }
    }
}

