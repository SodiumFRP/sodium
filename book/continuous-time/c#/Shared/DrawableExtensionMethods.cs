namespace Shared
{
    public static class DrawableExtensionMethods
    {
        public static DrawableDelegate Append(this DrawableDelegate first, DrawableDelegate second)
        {
            return (d, h, o, s) =>
            {
                first(d, h, o, s);
                second(d, h, o, s);
            };
        }
    }
}