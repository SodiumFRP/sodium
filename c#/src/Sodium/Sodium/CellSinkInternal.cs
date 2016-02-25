namespace Sodium
{
    public abstract class CellSinkInternal<T> : Cell<T>
    {
        public readonly StreamSink<T> StreamSink;

        internal CellSinkInternal(StreamSink<T> streamSink, T initialValue)
            : base(streamSink, initialValue)
        {
            this.StreamSink = streamSink;
        }
    }
}