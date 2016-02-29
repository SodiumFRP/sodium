using System;

namespace PetrolPump
{
    public static class Formatters
    {
        public static string FormatPrice(double price, int? maxDigits)
        {
            if (maxDigits == null)
            {
                return price.ToString("#0.000");
            }

            string priceIntString = Convert.ToInt64(Math.Floor(price)).ToString();
            int decimalDigits = Math.Min(Math.Max(maxDigits.Value - priceIntString.Length, 0), 3);
            return price.ToString("#0" + (decimalDigits > 0 ? ".".PadRight(decimalDigits + 1, '0') : string.Empty));
        }

        public static string FormatSaleCost(double cost)
        {
            return cost.ToString("#0.00");
        }

        public static string FormatSaleQuantity(double quantity)
        {
            return quantity.ToString("#0.00");
        }

        public static string FormatPresetAmount(int presetAmount)
        {
            return FormatSaleCost(presetAmount);
        }
    }
}