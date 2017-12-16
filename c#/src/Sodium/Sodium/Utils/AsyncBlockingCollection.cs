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
                yield return
                    autoResetEvent.WaitAsync().ContinueWith(t => TakeItem());

                //it is not really important to lock before getting Count
                //the iterator will catch up any items missed here
                while (collection.Count != 0)
                {
                    yield return Task.FromResult(TakeItem());
                }
            }
        }

        public void Add(TValue value)
        {
            lock (syncLock)
            {
                collection.Enqueue(value);
            }
            autoResetEvent.Set();
        }

        private TValue TakeItem()
        {
            lock (syncLock)
            {
                return collection.Dequeue();
            }
        }
    }
}
