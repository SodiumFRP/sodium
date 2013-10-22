/**
 * Note: This program uses the Java SE 8 feature of lambdas.
 *
 * If Java SE 8 hasn't been released yet, then you can use a lambda-enabled
 * prototype Java Development Kit from http://openjdk.java.net/projects/lambda/ 
 */
import sodium.*;
import java.awt.Container;
import java.awt.ItemSelectable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;
import javax.swing.*;

class Date implements Comparable<Date> {
    public Date(int year, int month, int day)
    {
        this.year = year;
        this.month = month;
        this.day = day;
    }

    public int year;
    public int month;
    public int day;
    
    public String toString() {
        return Integer.toString(year)+"-"+Integer.toString(month)+"-"+Integer.toString(day);
    }
    
    public int compareTo(Date o)
    {
        if (year < o.year) return -1;
        if (year > o.year) return 1;
        if (month < o.month) return -1;
        if (month > o.month) return 1;
        if (day < o.day) return -1;
        if (day > o.day) return 1;
        return 0;
    }
}

class Booking {
    /**
     * Return booking.
     */
    public Booking(Date start, Date end)
    {
        this.start = start;
        this.end = end;
    }
    
    /**
     * One-way booking.
     */
    public Booking(Date start)
    {
        this.start = start;
    }

    public boolean isReturn() { return end != null; }
    public boolean isOneWay() { return !isReturn(); }

    public Date start;
    public Date end;  // null means this is a one-way flight

    public String toString() {
        return isReturn() ? "return flight "+start+" to "+end
                          : "one-way flight "+start;
    }
}

class SwingHelpers {
    /**
     * A behavior giving the currently selected value of the combo box. 
     */
    public static <A> Behavior<A> comboValue(JComboBox<A> sel)
    {
        BehaviorSink<A> value = new BehaviorSink<A>((A)sel.getSelectedObjects()[0]);
        sel.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED)
                    value.send((A)e.getItem());
            }
        });
        return value;
    }

    /**
     * An event that is fired when the button is clicked.
     */
    public static Event<Unit> buttonClicked(JButton button)
    {
        EventSink<Unit> clicked = new EventSink<Unit>();
        button.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                clicked.send(Unit.unit);
            }
        });
        return clicked;
    }
};

/**
 * A reactive-enabled button.
 */
class Button extends JButton
{
    public Button(Behavior<Boolean> enabled, String text)
    {
        super(text);
        l = enabled.value().listen(e -> {
            setEnabled(e);
        });
        clicked = SwingHelpers.buttonClicked(this);
    }

    public Event<Unit> clicked;

    private Listener l;

    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }
}

class DateField extends Container
{
    private String[] months = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

    public DateField(Behavior<Boolean> enabled)
    {
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

        Integer[] days = new Integer[31];
        for (int i = 0; i < days.length; i++)
            days[i] = i+1;
        JComboBox<Integer> dayBox = new JComboBox<Integer>(days);
        add(dayBox);
        Behavior<Integer> day = SwingHelpers.comboValue(dayBox);

        JComboBox<String> monthBox = new JComboBox<String>(months);
        add(monthBox);
        Behavior<Integer> month = SwingHelpers.comboValue(monthBox).map(s -> {
            for (int i = 0; i < months.length; i++)
                if (s.equals(months[i]))
                    return i+1;
            throw new RuntimeException("Unknown month string "+s);
        });

        Integer[] years = new Integer[20];
        for (int i = 0; i < years.length; i++)
            years[i] = i+2012;
        JComboBox<Integer> yearBox = new JComboBox<Integer>(years);
        add(yearBox);

        Behavior<Integer> year = SwingHelpers.comboValue(yearBox);

        date = Behavior.lift((y,m,d) -> new Date(y,m,d), year, month, day);

        l = enabled.value().listen(e -> {
            yearBox.setEnabled(e);
            monthBox.setEnabled(e);
            dayBox.setEnabled(e);
        });
    }

    private Listener l;

    public void removeNotify() {
        l.unlisten();
        super.removeNotify();
    }

    public Behavior<Date> date;
}

public class FlightBooking extends JFrame
{
    public FlightBooking()
    {
        super("Book your flight");
        Container c = getContentPane();
        c.setLayout(new BoxLayout(c, BoxLayout.Y_AXIS));

        String[] ticketTypes = new String[] {"return flight", "one-way flight"};
        JComboBox<String> ticketType = new JComboBox<String>(ticketTypes);
        add(ticketType);

        // A boolean behavior indicating whether 'return flight' is currently selected.
        Behavior<Boolean> isReturn = SwingHelpers.comboValue(ticketType).map(ty -> ty.equals(ticketTypes[0]));

        DateField start = new DateField(new Behavior<Boolean>(true));
        c.add(start);

        DateField end = new DateField(isReturn);
        c.add(end);

        Behavior<Booking> booking = Behavior.lift(
            (isRet, st, en) -> isRet ? new Booking(st, en)
                                     : new Booking(st),
            isReturn, start.date, end.date);

        // One-way flights are always valid.
        // Return flights are valid if the return date is after the departure date.
        Behavior<Boolean> valid = booking.map(b ->
            b.isOneWay() || b.start.compareTo(b.end) < 0
        );

        Button bookButton = new Button(valid, "Book!");
        c.add(bookButton);

        // Snapshot the value of the booking when the button is clicked.
        booked = bookButton.clicked.snapshot(booking, (clicked, bking) -> bking);
    }

    /**
     * An event that fires when "OK" is clicked giving the chosen booking details.
     */
    public Event<Booking> booked;

    public static void main(String[] args)
    {
        FlightBooking booking = new FlightBooking();
        Listener l = booking.booked.listen(bk -> { System.out.println(bk); });
        booking.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                l.unlisten();
                System.exit(0);
            }
        });
        booking.pack();
        booking.setVisible(true);
    }
}

