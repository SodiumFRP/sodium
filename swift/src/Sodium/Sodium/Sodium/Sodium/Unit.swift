/// <summary>
///     A class representing the unit type (similar to <code>void</code>).
/// </summary>
public final class Unit: Equatable
{
    /// <summary>
    ///     The singleton value of type <see cref="Unit" />.
    /// </summary>
    public static let value = Unit()

    private init() {
    }

    var hashValue : Int { return 1 }
}

public func ==(lhs: Unit, rhs: Unit) -> Bool {
    return rhs === lhs
}
