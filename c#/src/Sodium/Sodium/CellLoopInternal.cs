namespace Sodium
{
    public abstract class CellLoopInternal<T> : LazyCell<T>
    {
        public readonly StreamLoop<T> StreamLoop;

        internal CellLoopInternal(StreamLoop<T> streamLoop)
            : base(streamLoop, null)
        {
            this.StreamLoop = streamLoop;
        }
    }
}