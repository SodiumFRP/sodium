namespace Sodium.Frp
{
    internal sealed class UnitInternal
    {
        internal static readonly UnitInternal Value = new UnitInternal();

        private UnitInternal()
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

        public static bool operator ==(UnitInternal x, UnitInternal y) => ReferenceEquals(x, null) == ReferenceEquals(y, null);

        public static bool operator !=(UnitInternal x, UnitInternal y) => ReferenceEquals(x, null) != ReferenceEquals(y, null);
    }
}