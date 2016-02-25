namespace Sodium
{
    internal class MaybeValue<T>
    {
        private IMaybe<T> value = Maybe.Nothing<T>();

        internal void Set(T value) => this.value = Maybe.Just(value);
        internal void Reset() => this.value = Maybe.Nothing<T>();
        internal IMaybe<T> Get() => this.value;
    }
}