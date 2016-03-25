import javax.swing.*;
import java.awt.*;
import java.util.Calendar;
import swidgets.*;
import nz.sodium.*;

class Rule {
    public Rule(Lambda2<Calendar, Calendar, Boolean> f) {
        this.f = f;
    }
    public final Lambda2<Calendar, Calendar, Boolean> f;
    public Cell<Boolean> reify(Cell<Calendar> dep, Cell<Calendar> ret) {
        return dep.lift(ret, f);
    }
    public Rule and(Rule other) {
        return new Rule(
            (d, r) -> this.f.apply(d, r) && other.f.apply(d, r)
        );
    }
}

public class airline2 {
    private static boolean unlucky(Calendar dt) {
        int day = dt.get(Calendar.DAY_OF_MONTH);
        return day == 4 || day == 14 || day == 24;
    }
    public static void main(String[] args) {
        JFrame view = new JFrame("airline2");
        view.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        SDateField dep = new SDateField();
        SDateField ret = new SDateField();
        Rule r1 = new Rule((d, r) -> d.compareTo(r) <= 0);
        Rule r2 = new Rule((d, r) -> !unlucky(d) && !unlucky(r));
        Rule r = r1.and(r2);
        Cell<Boolean> valid = r.reify(dep.date, ret.date);
        SButton ok = new SButton("OK", valid);

        GridBagLayout gridbag = new GridBagLayout();
        view.setLayout(gridbag);
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridwidth = 1;
        c.gridheight = 1;
        c.gridx = 0;
        c.gridy = 0;
        c.weightx = 0.0;
        view.add(new JLabel("departure"), c);
        c.gridx = 1;
        c.gridy = 0;
        c.weightx = 1.0;
        view.add(dep, c);
        c.gridwidth = 1;
        c.gridheight = 1;
        c.gridx = 0;
        c.gridy = 1;
        c.weightx = 0.0;
        view.add(new JLabel("return"), c);
        c.gridx = 1;
        c.gridy = 1;
        c.weightx = 1.0;
        view.add(ret, c);
        c.fill = GridBagConstraints.NONE;
        c.gridwidth = 2;
        c.gridx = 0;
        c.gridy = 2;
        c.weightx = 1.0;
        view.add(ok, c);
        view.setSize(380, 140);
        view.setVisible(true);
    }
}

