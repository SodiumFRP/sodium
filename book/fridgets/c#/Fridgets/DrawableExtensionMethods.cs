namespace Fridgets
{
    public static class DrawableExtensionMethods
    {
        public static DrawableDelegate Append(this DrawableDelegate first, DrawableDelegate second)
        {
            return d =>
            {
                first(d);
                second(d);
            };
        }
    }
}