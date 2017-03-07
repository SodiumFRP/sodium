using System;
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
    public static class MemoryManager
    {
        private static readonly TimeSpan Infinite = TimeSpan.FromMilliseconds(-1);
        private static readonly object StreamsLock = new object();
        private static readonly HashSet<StreamListeners> Streams = new HashSet<StreamListeners>();
        private static readonly object TimerIntervalLock = new object();
        private static TimeSpan? timerInterval;
        private static readonly object TimerLock = new object();
        private static readonly Timer Timer = new Timer(OnTimer, null, Infinite, Infinite);

        static MemoryManager()
        {
            StartCleaningUpAutomatically(TimeSpan.FromSeconds(30));
        }

        /// <summary>
        ///     Starts automatic cleanup at the specified interval.
        /// </summary>
        /// <param name="interval">The interval between clean up runs.</param>
        public static void StartCleaningUpAutomatically(TimeSpan interval)
        {
            lock (TimerIntervalLock)
            {
                timerInterval = interval;
            }

            SetTimer(interval);
        }

        /// <summary>
        ///     Stops automatic cleanup.
        /// </summary>
        public static void StopCleaningUpAutomatically()
        {
            StopTimer();

            lock (TimerIntervalLock)
            {
                timerInterval = null;
            }
        }

        /// <summary>
        ///     Manually runs a cleanup.
        /// </summary>
        public static void Cleanup()
        {
            StopTimer();

            CleanupInternal();

            TimeSpan? interval;
            lock (TimerIntervalLock)
            {
                interval = timerInterval;
            }

            if (interval != null)
            {
                SetTimer(interval.Value);
            }
        }

        private static void SetTimer(TimeSpan interval)
        {
            lock (TimerLock)
            {
                Timer.Change(interval, TimeSpan.FromMilliseconds(-1));
            }
        }

        private static void StopTimer()
        {
            lock (TimerLock)
            {
                Timer.Change(Infinite, Infinite);
            }
        }

        private static void OnTimer(object state)
        {
            CleanupInternal();

            TimeSpan? interval;
            lock (TimerIntervalLock)
            {
                interval = timerInterval;
            }

            if (interval != null)
            {
                SetTimer(interval.Value);
            }
        }

        internal static void Add<T>(StreamListeners<T> stream)
        {
            lock (StreamsLock)
            {
                Streams.Add(stream);
            }
        }

        internal static void CleanupInternal()
        {
            GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced);
            List<StreamListeners> streamsToCleanup = new List<StreamListeners>();
            lock (StreamsLock)
            {
                Streams.RemoveWhere(stream =>
                {
                    if (stream.ReadyToUnlisten())
                    {
                        streamsToCleanup.Add(stream);

                        return true;
                    }

                    return false;
                });
            }

            foreach (StreamListeners stream in streamsToCleanup)
            {
                stream.Unlisten();
            }
        }

        internal abstract class StreamListeners
        {
            public abstract bool ReadyToUnlisten();
            public abstract void Unlisten();
        }

        internal class StreamListeners<T> : StreamListeners
        {
            private readonly WeakReference<Stream<T>> stream;
            private readonly List<IWeakListener> listeners;

            public StreamListeners(Stream<T> stream)
            {
                this.stream = new WeakReference<Stream<T>>(stream);
                this.listeners = new List<IWeakListener>();
            }

            public override bool ReadyToUnlisten()
            {
                Stream<T> temp;
                return !this.stream.TryGetTarget(out temp);
            }

            public override void Unlisten()
            {
                foreach (var l in this.listeners)
                {
                    l.Unlisten();
                }
            }

            public void AddListener(IWeakListener listener)
            {
                this.listeners.Add(listener);
            }
        }
    }
}
