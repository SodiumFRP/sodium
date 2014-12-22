package pump;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

public class Formatters {
    public static DecimalFormat priceFmt = new DecimalFormat("#0.0000000",
            new DecimalFormatSymbols(Locale.US));
    public static String formatPrice(double price)
    {
        int lcdSize = 4;
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

    public static DecimalFormat costFmt = new DecimalFormat("#0.00",
            new DecimalFormatSymbols(Locale.US));
    public static String formatSaleCost(double cost)
    {
        return costFmt.format(cost);
    }

    public static DecimalFormat quantityFmt = new DecimalFormat("#0.00",
            new DecimalFormatSymbols(Locale.US));
    public static String formatSaleQuantity(double quantity)
    {
        return quantityFmt.format(quantity);
    }
}

