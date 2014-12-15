using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PetrolPump.Pump
{
  public class Formatters
  {
    public static DecimalFormat priceFmt = new DecimalFormat("0.0000000",
            new CultureInfo("us-EN"));

    public static String FormatPrice(double price)
    {
        int lcdSize = 4;
        String text = priceFmt.Format(price);
        int i = 0;
        int digits = 0;
        while (true) {
            while (i < text.Length && text[i] == '.') i++;
            if (digits == lcdSize) return text.Substring(0, i);
            if (i >= text.Length) break;
            digits++;
            i++;
        }
        return text;
    }

    public static DecimalFormat costFmt = new DecimalFormat("0.00",
            new CultureInfo("us-EN"));
    public static String FormatSaleCost(double cost)
    {
        return costFmt.Format(cost);
    }

    public static DecimalFormat quantityFmt = new DecimalFormat("#0.00",
            new CultureInfo("us-EN"));
    public static String FormatSaleQuantity(double quantity)
    {
        return quantityFmt.Format(quantity);
    }
}

  //Formats doubles, not decimals but in order to stay close to original code the name is DecimalFormat.
  public class DecimalFormat
  {
    string format;
    readonly CultureInfo cultureInfo;

    public DecimalFormat(string format, CultureInfo cultureInfo)
    {
      this.format = format;
      this.cultureInfo = cultureInfo;
    }

    public string Format(double number)
    {
      return string.Format(cultureInfo, format, number);
    }
  }
}