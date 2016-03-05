using System.Globalization;
using System.Windows;
using System.Windows.Media;

namespace Fridgets
{
    public static class FontUtilities
    {
        public static Size MeasureString(string s, Typeface typeface, double fontSize)
        {
            FormattedText formattedText = GetStandardFormattedText(s, typeface, fontSize, Brushes.Black);
            return new Size(formattedText.Width, formattedText.Height);
        }

        public static FormattedText GetStandardFormattedText(string s, Typeface typeface, double fontSize, Brush brush)
        {
            return new FormattedText(
                s,
                CultureInfo.CurrentUICulture,
                FlowDirection.LeftToRight,
                typeface,
                fontSize,
                brush);
        }
    }
}