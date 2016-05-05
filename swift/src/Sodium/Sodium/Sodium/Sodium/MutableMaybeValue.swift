public class MutableMaybeValue<T>
{
    private var value = Maybe.Nothing<T>()

    public func Set(value: T) { self.value = Maybe.Just(value) }
    public func Reset() { self.value = Maybe.Nothing<T>() }
    public func Get() -> IMaybe<T> { return self.value }
}
