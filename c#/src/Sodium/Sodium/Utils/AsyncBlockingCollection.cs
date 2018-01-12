using System.Collections.Generic;
using System.Threading.Tasks;

namespace Sodium.Utils
{
    internal class AsyncBlockingCollection<TValue>
    {
        private readonly Queue<TValue> collection = new Queue<TValue>();
        private readonly AsyncAutoResetEvent autoResetEvent = new AsyncAutoResetEvent();
        private readonly object syncLock = new object();

        public IEnumerable<Task<TValue>> GetConsumingEnumerable()
        {
            while (true)
            {
                yield return this.Wait();

                Result result;
                do
                {
                    result = this.TakeItem();
                    if (result.HasValue)
                    {
                        yield return Task.FromResult(result.Value);
                    }
                }
                while (result.HasValue);
            }
            // ReSharper disable once IteratorNeverReturns
        }

        private async Task<TValue> Wait()
        {
            Result result;
            do
            {
                await this.autoResetEvent.WaitAsync().ConfigureAwait(false);
                result = this.TakeItem();
            }
            while (!result.HasValue);

            return result.Value;
        }

        public void Add(TValue value)
        {
            lock (this.syncLock)
            {
                this.collection.Enqueue(value);
            }

            this.autoResetEvent.Set();
        }

        private Result TakeItem()
        {
            lock (this.syncLock)
            {
                return this.collection.Count > 0 ? new Result(this.collection.Dequeue()) : new Result();
            }
        }

        private struct Result
        {
            public readonly bool HasValue;
            public readonly TValue Value;

            public Result(TValue value)
            {
                this.HasValue = true;
                this.Value = value;
            }
        }
    }
}