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
                yield return this.autoResetEvent.WaitAsync().ContinueWith(t => this.TakeItem());

                //it is not really important to lock before getting Count
                //the iterator will catch up any items missed here
                // ReSharper disable once InconsistentlySynchronizedField
                while (this.collection.Count != 0)
                {
                    yield return Task.FromResult(this.TakeItem());
                }
            }
            // ReSharper disable once IteratorNeverReturns
        }

        public void Add(TValue value)
        {
            lock (this.syncLock)
            {
                this.collection.Enqueue(value);
            }
            this.autoResetEvent.Set();
        }

        private TValue TakeItem()
        {
            lock (this.syncLock)
            {
                return this.collection.Dequeue();
            }
        }
    }
}