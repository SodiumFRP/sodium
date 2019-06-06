using System.Collections.Generic;
using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump
{
    public static class CellExtensionMethods
    {
        public static Stream<T> Changes<T>(this Cell<T> cell) =>
            cell.Values().Snapshot(cell, (n, o) => EqualityComparer<T>.Default.Equals(o, n) ? Maybe.None : Maybe.Some(n)).FilterMaybe();
    }
}