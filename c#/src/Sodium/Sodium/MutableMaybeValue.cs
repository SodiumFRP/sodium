namespace Sodium
{
    public class MutableMaybeValue<T>
    {
        private IMaybe<T> value = Maybe.Nothing<T>();

        public void Set(T value) => this.value = Maybe.Just(value);
        public void Reset() => this.value = Maybe.Nothing<T>();
        public IMaybe<T> Get() => this.value;
    }
}