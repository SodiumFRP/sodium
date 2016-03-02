namespace Shift2
{
    public static class DoubleExtensionMethods
    {
        public static double ZeroIfNaN(this double d) => double.IsNaN(d) ? 0 : d;
    }
}