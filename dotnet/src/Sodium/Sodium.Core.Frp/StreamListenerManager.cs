#if NETSTANDARD2_0 || NET45
using System.Threading;
#else
using System.Threading.Tasks;
#endif
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Sodium.Frp
{
    /// <summary>
    ///     Provides methods to clean up after objects which have gone out of scope.
    /// </summary>
    /// <remarks>
    ///     This class checks for any streams which have been garbage collected and cleans them up by running
    ///     <see cref="IListener.Unlisten" /> on each of their attached listeners.
    ///     By default, this class automatically cleans up every 30 seconds.
    /// </remarks>
    internal static class StreamListenerManager
    {
        private static Dictionary<Guid, StreamListeners> streamsById = new Dictionary<Guid, StreamListeners>();
        private static long streamsByIdCapacity;
        private static readonly object StreamsByIdLock = new object();
        private static readonly BlockingCollection<Guid> StreamIdsToRemove = new BlockingCollection<Guid>();
        private static readonly ConcurrentQueue<Guid> StreamIdsToRemoveLastChance = new ConcurrentQueue<Guid>();

        static StreamListenerManager()
        {
#if NETSTANDARD2_0 || NET45
            Thread cleanupThread = new Thread(SodiumCleanup)
            {
                Name = "Sodium Cleanup Thread",
                IsBackground = true
            };
            cleanupThread.Start();

            Thread lastChanceCleanupThread = new Thread(SodiumLastChanceCleanup)
            {
                Name = "Sodium Last Chance Cleanup Thread",
                IsBackground = true
            };
            lastChanceCleanupThread.Start();
#else
            Task.Factory.StartNew(SodiumCleanup, TaskCreationOptions.LongRunning | TaskCreationOptions.DenyChildAttach);
            Task.Factory.StartNew(SodiumLastChanceCleanup, TaskCreationOptions.LongRunning | TaskCreationOptions.DenyChildAttach);
#endif
        }

        private static void SodiumCleanup()
        {
            foreach (Guid streamId in StreamIdsToRemove.GetConsumingEnumerable())
            {
                StreamListeners streamListeners;
                bool found;
                lock (StreamsByIdLock)
                {
                    found = streamsById.TryGetValue(streamId, out streamListeners);
                }

                if (found)
                {
                    streamListeners.Unlisten();
                }
                else
                {
                    StreamIdsToRemoveLastChance.Enqueue(streamId);
                }
            }
        }

#if NETSTANDARD2_0 || NET45
        private static void SodiumLastChanceCleanup()
#else
        private static async Task SodiumLastChanceCleanup()
#endif
        {
            while (true)
            {
#if NETSTANDARD2_0 || NET45
                Thread.Sleep(30000);
#else
                await Task.Delay(30000);
#endif

                while (StreamIdsToRemoveLastChance.TryDequeue(out Guid streamId))
                {
                    StreamListeners streamListeners;
                    bool found;
                    lock (StreamsByIdLock)
                    {
                        found = streamsById.TryGetValue(streamId, out streamListeners);
                    }

                    if (found)
                    {
                        streamListeners.Unlisten();
                    }
                }
            }
            // ReSharper disable once FunctionNeverReturns
        }

        public static void Remove(Guid streamId)
        {
            StreamIdsToRemove.Add(streamId);
        }

        internal class StreamListeners
        {
            private readonly Guid streamId;
            private readonly List<IListenerWithWeakReference> listeners;

            public StreamListeners(Guid streamId)
            {
                this.streamId = streamId;
                this.listeners = new List<IListenerWithWeakReference>();

                lock (StreamsByIdLock)
                {
                    streamsById.Add(streamId, this);
                    streamsByIdCapacity++;
                }
            }

            internal void Unlisten()
            {
                foreach (IListenerWithWeakReference l in this.listeners)
                {
                    l.Unlisten();
                }

                lock (StreamsByIdLock)
                {
                    streamsById.Remove(this.streamId);
                    // Dictionary does not reclaim space after items are removed, so we will create a new one if we can reclaim a substantial amount of space
                    if (streamsByIdCapacity > 100 && streamsById.Count < streamsByIdCapacity / 2)
                    {
                        streamsById = new Dictionary<Guid, StreamListeners>(streamsById);
                        streamsByIdCapacity = streamsById.Count;
                    }
                }
            }

            internal void AddListener(IListenerWithWeakReference listener) => this.listeners.Add(listener);
        }
    }
}