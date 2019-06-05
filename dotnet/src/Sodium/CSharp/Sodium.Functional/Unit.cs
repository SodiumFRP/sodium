namespace Sodium.Functional
{
    /// <summary>
    ///     A class representing the unit type (similar to <code>void</code>).
    /// </summary>
    public sealed class Unit
    {
        /// <summary>
        ///     The singleton value of type <see cref="Unit" />.
        /// </summary>
        public static readonly Unit Value = new Unit();

        private Unit()
        {
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj))
            {
                return false;
            }

            if (ReferenceEquals(this, obj))
            {
                return true;
            }

            if (obj.GetType() != this.GetType())
            {
                return false;
            }

            return true;
        }

        public override int GetHashCode() => 1;

        public static bool operator ==(Unit x, Unit y) => ReferenceEquals(x, null) == ReferenceEquals(y, null);

        public static bool operator !=(Unit x, Unit y) => ReferenceEquals(x, null) != ReferenceEquals(y, null);
    }
}