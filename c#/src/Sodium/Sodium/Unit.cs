namespace Sodium
{
    /// <summary>
    ///     A class representing the unit type (similar to <code>void</code>).
    /// </summary>
    public class Unit
    {
        /// <summary>
        ///     The singleton value of type <see cref="Unit" />.
        /// </summary>
        public static readonly Unit Value = new Unit();

        private Unit()
        {
        }
    }
}