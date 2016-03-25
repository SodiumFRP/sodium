package swidgets;

import nz.sodium.*;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Optional;
import java.util.Vector;
import java.awt.FlowLayout;
import javax.swing.JComponent;

public class SDateField extends JComponent {
    public SDateField() {
        this(new GregorianCalendar());
    }
    private static final String[] months = new String[] {
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    };
    public SDateField(Calendar cal) {
        Vector<Integer> years = new Vector<>();
        Calendar now = new GregorianCalendar();
        for (int y = now.get(Calendar.YEAR) - 10; y <= now.get(Calendar.YEAR) + 10; y++)
            years.add(y);
        SComboBox<Integer> year = new SComboBox<Integer>(years);
        year.setSelectedItem(cal.get(Calendar.YEAR));
        SComboBox<String> month = new SComboBox<String>(months);
        Vector<Integer> days = new Vector<>();
        for (int d = 1; d <= 31; d++)
            days.add(d);
        month.setSelectedItem(months[cal.get(Calendar.MONTH)]);
        SComboBox<Integer> day = new SComboBox<Integer>(days);
        day.setSelectedItem(cal.get(Calendar.DAY_OF_MONTH));
        setLayout(new FlowLayout());
        add(year);
        add(month);
        add(day);
        Cell<Optional<Integer>> monthIndex = month.selectedItem.map(
            ostr -> {
                if (ostr.isPresent()) {
                    for (int i = 0; i < months.length; i++)
                        if (months[i].equals(ostr.get()))
                            return Optional.of(i);
                }
                return Optional.empty();
            });
        date = year.selectedItem.lift(monthIndex, day.selectedItem,
        	(oy, om, od) -> {
					return oy.isPresent() && om.isPresent() && od.isPresent()
						? new GregorianCalendar(oy.get(), om.get(), od.get())
						: new GregorianCalendar();
				});
    }
    public final Cell<Calendar> date;
}

