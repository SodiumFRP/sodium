namespace Battle
{
    public static class DoubleExtensionMethods
    {
        public static double ZeroIfNaN(this double d) => double.IsNaN(d) ? 0 : d;
    }
}