using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using NUnit.Framework;

namespace Sodium.Frp.Tests.Memory
{
    /// <summary>
    ///     Lifetime tests that run under plain <c>dotnet test</c>.
    /// </summary>
    /// <remarks>
    ///     <para>
    ///         The tests in <see cref="StreamTests" /> cover the same ground but count live objects with
    ///         dotMemory, so they are all <c>[Ignore]</c>d and never run in CI. These assert the same
    ///         invariants using only weak references and the node's own listener set, so they actually
    ///         guard the cleanup machinery on every build.
    ///     </para>
    ///     <para>
    ///         Two things are deliberately not asserted. First, that a node is still connected immediately
    ///         after a collection: <see cref="StreamListenerManager" /> unhooks nodes from a background
    ///         thread, so whether that has happened yet is a race. Second, which of the two cleanup paths
    ///         did the work - the background thread, or the lazy pruning in <c>Stream.Send</c> when a
    ///         target's weak reference has died. What matters is that the node ends up disconnected, so
    ///         these tests send a value to force a deterministic outcome and check that.
    ///     </para>
    /// </remarks>
    [TestFixture]
    public class GarbageCollectionTests
    {
        [Test]
        public void MappedStreamIsCollectedOnceDroppedAndUnlistened()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            List<string> @out = new List<string>();

            WeakReference mapped = CreateMappedStreamAndUnlisten(s, @out);

            CollectionAssert.AreEqual(new[] { "3" }, @out, "the mapped stream should have fired while it was listening");

            Collect();

            Assert.IsFalse(
                mapped.IsAlive,
                "nothing should still root a mapped stream after the caller drops it and unlistens");
        }

        [Test]
        public void SourceNodeIsDisconnectedAfterDownstreamIsCollected()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            List<string> @out = new List<string>();

            Assert.AreEqual(0, s.Node.GetListenersCopy().Count, "a fresh sink has no listeners");

            WeakReference mapped = CreateMappedStreamAndUnlisten(s, @out);

            Collect();

            Assert.IsFalse(mapped.IsAlive, "the mapped stream should have been collected");

            // Sending is what makes this deterministic: either the cleanup thread already unhooked
            // the node, or this send prunes the target whose weak reference has died.
            s.Send(2);

            Assert.AreEqual(
                0,
                s.Node.GetListenersCopy().Count,
                "the source node should no longer be linked to a collected downstream stream");
        }

        [Test]
        public void EveryStreamInAChainIsCollected()
        {
            const int depth = 10;

            StreamSink<int> s = Stream.CreateSink<int>();
            WeakReference[] chain = CreateChainAndUnlisten(s, depth);

            Collect();

            for (int i = 0; i < chain.Length; i++)
            {
                Assert.IsFalse(chain[i].IsAlive, "stream at depth {0} should have been collected", i);
            }

            s.Send(1);

            Assert.AreEqual(
                0,
                s.Node.GetListenersCopy().Count,
                "the source node should be disconnected once the whole chain is gone");
        }

        [Test]
        public void ListenerIsKeptAliveWhileStillListening()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            List<int> @out = new List<int>();

            WeakReference listener = CreateListenerAndDropTheReference(s, @out);

            Collect();

            // This is deliberate, not an oversight: Listen roots the listener in the stream's
            // keep-alive set precisely so that a caller which ignores the return value still
            // receives values. Losing this would make listeners silently stop firing.
            Assert.IsTrue(listener.IsAlive, "an active listener should stay alive even once the caller drops it");

            s.Send(5);

            CollectionAssert.AreEqual(new[] { 5 }, @out, "a rooted listener should still be firing");
        }

        [Test]
        public void UnlistenReleasesTheListener()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            List<int> @out = new List<int>();

            WeakReference listener = CreateListenerAndUnlisten(s, @out);

            Collect();

            Assert.IsFalse(listener.IsAlive, "Unlisten should stop the listener being rooted by the stream");

            s.Send(5);

            CollectionAssert.IsEmpty(@out, "an unlistened listener should not fire");
        }

        [Test]
        public void CollectedStreamsAreReapedFromTheRegistry()
        {
            // StreamListenerManager tracks every stream ever created, so if the sweep failed to
            // reap collected ones the registry would grow without bound. Nothing else here would
            // notice: the node-level tests above pass either way, because Stream.Send prunes dead
            // targets on its own.
            Collect();
            StreamListenerManager.Sweep();
            int before = StreamListenerManager.RegistryCount;

            CreateGarbageStreams(30);

            Collect();
            StreamListenerManager.Sweep();
            int after = StreamListenerManager.RegistryCount;

            Assert.That(
                after,
                Is.LessThanOrEqualTo(before),
                "the registry should be back to its previous size once the streams it tracked are collected");
        }

        // Each of these runs in its own non-inlined method so the locals are certainly out of scope
        // by the time the caller collects, whatever the JIT decides to keep alive.

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static void CreateGarbageStreams(int count)
        {
            for (int i = 0; i < count; i++)
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                Stream<int> mapped = s.Map(v => v + 1);
                IListener listener = mapped.Listen(_ => { });
                listener.Unlisten();
            }
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static WeakReference CreateMappedStreamAndUnlisten(StreamSink<int> s, List<string> @out)
        {
            Stream<string> mapped = s.Map(x => (x + 2).ToString());
            IListener listener = mapped.Listen(@out.Add);
            s.Send(1);
            listener.Unlisten();
            return new WeakReference(mapped);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static WeakReference[] CreateChainAndUnlisten(StreamSink<int> s, int depth)
        {
            List<WeakReference> chain = new List<WeakReference>();
            Stream<int> current = s;
            for (int i = 0; i < depth; i++)
            {
                current = current.Map(v => v + 1);
                chain.Add(new WeakReference(current));
            }

            IListener listener = current.Listen(_ => { });
            listener.Unlisten();
            return chain.ToArray();
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static WeakReference CreateListenerAndDropTheReference(StreamSink<int> s, List<int> @out)
        {
            IListener listener = s.Listen(@out.Add);
            return new WeakReference(listener);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static WeakReference CreateListenerAndUnlisten(StreamSink<int> s, List<int> @out)
        {
            IListener listener = s.Listen(@out.Add);
            listener.Unlisten();
            return new WeakReference(listener);
        }

        private static void Collect()
        {
            // Twice, with finalizers in between: Stream still has a finalizer today, so the first
            // pass only queues it. Once the finalizer is gone this stays correct, just quicker.
            GC.Collect();
            GC.WaitForPendingFinalizers();
            GC.Collect();
        }
    }
}
