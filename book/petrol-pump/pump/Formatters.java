package pump;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

public class Formatters {
    private static DecimalFormat priceFmt = new DecimalFormat("#0.0000000",
            new DecimalFormatSymbols(Locale.US));
    public static String formatPrice(double price, int lcdSize)
    {
        String text = priceFmt.format(price);
        int i = 0;
        int digits = 0;
        while (true) {
            while (i < text.length() && text.charAt(i) == '.') i++;
            if (digits == lcdSize) return text.substring(0, i);
            if (i >= text.length()) break;
            digits++;
            i++;
        }
        return text;
    }
}

