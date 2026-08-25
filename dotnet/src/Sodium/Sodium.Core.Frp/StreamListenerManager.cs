using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;

namespace Sodium.Frp
{
    /// <summary>
    ///     Provides methods to clean up after streams which have gone out of scope.
    /// </summary>
    /// <remarks>
    ///     <para>
    ///         Every stream registers a <see cref="StreamListeners" /> holding a weak handle back to it. A
    ///         background thread sweeps the registry; any entry whose stream has been collected has its
    ///         attached listeners unlistened, which disconnects its node from upstream.
    ///     </para>
    ///     <para>
    ///         The sweep is paced by the collector rather than by a timer.
    ///         <see cref="GcSweepTrigger" /> is a single finalizable object for the whole process - not one
    ///         per stream, which is the cost this design exists to avoid - that wakes the sweeper after
    ///         every collection. It only signals; the sweeping happens on the background thread, so nothing
    ///         that takes a node lock ever runs on the finalizer thread, where blocking would stall
    ///         finalization process-wide. Without this the registry held the bookkeeping for dead streams
    ///         until the next timed pass, measured at roughly 10MB after dropping 22,000 streams.
    ///     </para>
    ///     <para>
    ///         The handle is a raw weak <see cref="GCHandle" /> rather than a
    ///         <see cref="System.WeakReference" /> deliberately. WeakReference owns a handle it has to free
    ///         in a finalizer, so using one per stream would cost about as much as the
    ///         <c>~Stream</c> finalizer this design replaced - measured at roughly 150ns per stream either
    ///         way, against 50ns for the bare handle. Anything reintroducing a finalizable object per
    ///         stream gives the saving straight back.
    ///     </para>
    ///     <para>
    ///         This sweep is not the primary cleanup path and does not need to be prompt.
    ///         <see cref="Stream{T}.Send" /> prunes any target whose weak reference has died as it walks
    ///         the listener set, so a node linked to a collected stream is disconnected the next time it
    ///         fires regardless. The sweep exists to catch the streams that never fire again.
    ///     </para>
    /// </remarks>
    internal static class StreamListenerManager
    {
        // A backstop only, hence the length of it. Entries become reapable when a stream is
        // collected, which only happens at a garbage collection, and every collection signals a
        // sweep - so in a healthy process this interval never finds anything to do. It exists for
        // the case where the signal stops arriving at all, most plausibly a finalizer thread wedged
        // by unrelated code, where the sweeper thread is unaffected and can still make progress.
        private const int TimedSweepIntervalInMilliseconds = 300000;

        private static readonly object RegistryLock = new object();
        private static readonly List<StreamListeners> Registry = new List<StreamListeners>();
        private static readonly AutoResetEvent SweepRequested = new AutoResetEvent(false);

        static StreamListenerManager()
        {
            Thread cleanupThread = new Thread(SodiumCleanup)
            {
                Name = "Sodium Cleanup Thread",
                IsBackground = true
            };
            cleanupThread.Start();

            // Deliberately not stored anywhere: it has to be unreachable for its finalizer to run.
            // ReSharper disable once ObjectCreationAsStatement
            new GcSweepTrigger();
        }

        private static void SodiumCleanup()
        {
            while (true)
            {
                // Woken by a collection, or by the backstop interval if a signal never arrives.
                SweepRequested.WaitOne(TimedSweepIntervalInMilliseconds);
                Sweep();
            }
            // ReSharper disable once FunctionNeverReturns
        }

        /// <summary>
        ///     Asks the cleanup thread to sweep after each garbage collection, by being collected itself
        ///     and re-registering. One instance exists for the whole process.
        /// </summary>
        private sealed class GcSweepTrigger
        {
            ~GcSweepTrigger()
            {
                try
                {
                    SweepRequested.Set();
                }
                catch
                {
                    // A finalizer must never throw, and there is nothing useful to do if signalling
                    // fails - the timed pass will pick the work up regardless.
                }
                finally
                {
                    if (!Environment.HasShutdownStarted && !AppDomain.CurrentDomain.IsFinalizingForUnload())
                    {
                        // A fresh instance rather than GC.ReRegisterForFinalize(this): resurrecting
                        // this one promotes it, after which young collections no longer see it and
                        // only gen1+ would fire the sweep. A newly allocated object starts in gen0,
                        // so the pacing follows every collection instead of only the older ones.
                        // ReSharper disable once ObjectCreationAsStatement
                        new GcSweepTrigger();
                    }
                }
            }
        }

        /// <summary>
        ///     How many streams the registry is currently tracking. For tests; the number is only
        ///     meaningful straight after a <see cref="Sweep" />.
        /// </summary>
        internal static int RegistryCount
        {
            get
            {
                lock (RegistryLock)
                {
                    return Registry.Count;
                }
            }
        }

        /// <summary>
        ///     Internal rather than private so that tests can drive a sweep directly instead of
        ///     waiting on the background thread's interval.
        /// </summary>
        internal static void Sweep()
        {
            List<StreamListeners> collected = null;

            lock (RegistryLock)
            {
                // Backwards, swapping the last entry into each gap. Everything swapped in comes from
                // a position already passed, so nothing is skipped and nothing is checked twice.
                for (int i = Registry.Count - 1; i >= 0; i--)
                {
                    StreamListeners entry = Registry[i];

                    if (entry.IsStreamAlive)
                    {
                        continue;
                    }

                    int last = Registry.Count - 1;
                    if (i != last)
                    {
                        Registry[i] = Registry[last];
                    }

                    Registry.RemoveAt(last);

                    if (collected == null)
                    {
                        collected = new List<StreamListeners>();
                    }

                    collected.Add(entry);
                }

                // A List keeps its backing array after removals, so reclaim it once a spike has drained.
                if (Registry.Capacity > 100 && Registry.Count < Registry.Capacity / 2)
                {
                    Registry.TrimExcess();
                }
            }

            // Released outside the registry lock: unlistening takes node locks, and running arbitrary
            // listener teardown while holding the registry would invite a lock ordering problem.
            if (collected != null)
            {
                foreach (StreamListeners entry in collected)
                {
                    entry.Release();
                }
            }
        }

        internal class StreamListeners
        {
            private readonly List<IListenerWithWeakReference> listeners = new List<IListenerWithWeakReference>();

            // Weak, so the registry never keeps a stream alive. Freed in Release.
            private GCHandle streamHandle;

            public StreamListeners(object stream)
            {
                this.streamHandle = GCHandle.Alloc(stream, GCHandleType.Weak);

                lock (RegistryLock)
                {
                    Registry.Add(this);
                }
            }

            internal bool IsStreamAlive => this.streamHandle.IsAllocated && this.streamHandle.Target != null;

            internal void AddListener(IListenerWithWeakReference listener) => this.listeners.Add(listener);

            internal void Release()
            {
                foreach (IListenerWithWeakReference l in this.listeners)
                {
                    l.Unlisten();
                }

                this.listeners.Clear();

                if (this.streamHandle.IsAllocated)
                {
                    this.streamHandle.Free();
                }
            }
        }
    }
}
