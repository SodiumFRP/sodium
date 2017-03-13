using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Threading;

namespace Sodium
{
    /// <summary>
    ///     Provides methods to clean up after objects which have gone out of scope.
    /// </summary>
    /// <remarks>
    ///     This class checks for any streams which have been garbage collected and cleans them up by running <see cref="IListener.Unlisten"/> on each of their attached listeners.
    ///     By default, this class automatically cleans up every 30 seconds.
    /// </remarks>
    internal static class StreamListenerManager
    {
        private static Dictionary<Guid, StreamListeners> StreamsById = new Dictionary<Guid, StreamListeners>();
        private static long StreamsByIdCapacity;
        private static readonly object StreamsByIdLock = new object();
        private static readonly BlockingCollection<Guid> StreamIdsToRemove = new BlockingCollection<Guid>();

        static StreamListenerManager()
        {
            Thread cleanupThread = new Thread(() =>
            {
                foreach (Guid streamId in StreamIdsToRemove.GetConsumingEnumerable())
                {
                    StreamListeners streamListeners;
                    lock (StreamsByIdLock)
                    {
                        streamListeners = StreamsById[streamId];
                    }
                    streamListeners.Unlisten();
                }
            })
            {
                Name = "Sodium Cleanup Thread",
                IsBackground = true
            };
            cleanupThread.Start();
        }

        public static void Remove(Guid streamId)
        {
            StreamIdsToRemove.Add(streamId);
        }

        internal class StreamListeners
        {
            private readonly Guid streamId;
            private readonly List<IWeakListener> listeners;

            public StreamListeners(Guid streamId)
            {
                this.streamId = streamId;
                this.listeners = new List<IWeakListener>();

                lock (StreamsByIdLock)
                {
                    StreamsById.Add(streamId, this);
                    StreamsByIdCapacity++;
                }
            }

            public void Unlisten()
            {
                foreach (IWeakListener l in this.listeners)
                {
                    l.Unlisten();
                }

                lock (StreamsByIdLock)
                {
                    StreamsById.Remove(this.streamId);
                    // Dictionary does not reclaim space after items are removed, so we will create a new one if we can reclaim a substantial amount of space
                    if (StreamsByIdCapacity > 100 && StreamsById.Count < StreamsByIdCapacity / 2)
                    {
                        StreamsById = new Dictionary<Guid, StreamListeners>(StreamsById);
                        StreamsByIdCapacity = StreamsById.Count;
                    }
                }
            }

            public void AddListener(IWeakListener listener)
            {
                this.listeners.Add(listener);
            }
        }
    }
}
