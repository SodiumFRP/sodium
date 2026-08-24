using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace Sodium.Frp.Async.Tests
{
    [TestFixture]
    public class AsyncConcurrencyStrategyFactoryTests
    {
        [Test]
        public void Parallel_BothStartImmediatelyAndPublishInCompletionOrder()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            ControlledOperation<string, string> op = new();
            List<string> received = new();
            IListener l = results.Listen(received.Add);

            AsyncMapStatus<string> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: op.Operation,
                strategy: AsyncConcurrencyStrategyFactory.Parallel("unused"),
                inputConverter: v => v,
                resultConverter: v => v);

            source.Send("a");
            source.Send("b");

            TestUtil.WaitUntil(() => op.HasStarted("a") && op.HasStarted("b"));

            // Both admitted and started before either is released — proves Parallel never waits.
            op.Release("b", "B");
            TestUtil.WaitUntil(() => received.Count == 1);
            op.Release("a", "A");
            TestUtil.WaitUntil(() => received.Count == 2);

            // Completion order, not submission order.
            CollectionAssert.AreEqual(new[] { "B", "A" }, received);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void Queue_SecondDoesNotStartUntilFirstCompletes()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            ControlledOperation<string, string> op = new();
            List<string> received = new();
            IListener l = results.Listen(received.Add);

            AsyncMapStatus<string> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: op.Operation,
                strategy: AsyncConcurrencyStrategyFactory.Queue<string>(),
                inputConverter: v => v,
                resultConverter: v => v);

            source.Send("a");
            source.Send("b");
            source.Send("c");

            TestUtil.WaitUntil(() => op.HasStarted("a"));
            Assert.IsFalse(op.HasStarted("b"), "b must stay queued while a is running.");
            Assert.IsFalse(op.HasStarted("c"), "c must stay queued while a is running.");

            op.Release("a", "A");
            TestUtil.WaitUntil(() => op.HasStarted("b"));
            Assert.IsFalse(op.HasStarted("c"), "c must stay queued while b is running.");

            op.Release("b", "B");
            TestUtil.WaitUntil(() => op.HasStarted("c"));

            op.Release("c", "C");
            TestUtil.WaitUntil(() => received.Count == 3);

            CollectionAssert.AreEqual(new[] { "A", "B", "C" }, received);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void QueuePerGroup_DifferentGroupsRunConcurrentlyButSameGroupSerializes()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            ControlledOperation<string, string> op = new();
            List<string> received = new();
            IListener l = results.Listen(received.Add);

            // Group is the character before the hyphen: "g1-a"/"g1-b" share a group, "g2-a" doesn't.
            string GetGroup(string v) => v.Split('-')[0];

            AsyncMapStatus<string> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: op.Operation,
                strategy: AsyncConcurrencyStrategyFactory.QueuePerGroup<string, string, string>(GetGroup),
                inputConverter: v => v,
                resultConverter: v => v);

            source.Send("g1-a");
            source.Send("g1-b");
            source.Send("g2-a");

            // g1-a and g2-a are in different groups, so both start; g1-b waits behind g1-a.
            TestUtil.WaitUntil(() => op.HasStarted("g1-a") && op.HasStarted("g2-a"));
            Assert.IsFalse(op.HasStarted("g1-b"), "g1-b shares a group with g1-a and must wait.");

            op.Release("g1-a", "A1");
            TestUtil.WaitUntil(() => op.HasStarted("g1-b"));

            op.Release("g1-b", "B1");
            op.Release("g2-a", "A2");
            TestUtil.WaitUntil(() => received.Count == 3);

            CollectionAssert.AreEquivalent(new[] { "A1", "B1", "A2" }, received);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void SwitchLatest_SupersededRunIsNeverPublished()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            ControlledOperation<string, string> op = new();
            List<string> received = new();
            IListener l = results.Listen(received.Add);

            AsyncMapStatus<string> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: op.Operation,
                strategy: AsyncConcurrencyStrategyFactory.SwitchLatest<string>(),
                inputConverter: v => v,
                resultConverter: v => v);

            source.Send("a");
            TestUtil.WaitUntil(() => op.HasStarted("a"));

            source.Send("b");
            TestUtil.WaitUntil(() => op.HasStarted("b"));

            // "a" is still in flight when it's superseded; releasing it must not publish.
            op.Release("a", "A");
            op.Release("b", "B");
            TestUtil.WaitUntil(() => received.Count == 1);

            // Give "a" a fair chance to have published if the supersede logic were broken.
            System.Threading.Thread.Sleep(100);

            CollectionAssert.AreEqual(new[] { "B" }, received);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void Queue_SameStrategyInstanceSharedAcrossTwoPipelinesDoesNotCrossSerialize()
        {
            AsyncConcurrencyStrategyBase<string, string> sharedQueue = AsyncConcurrencyStrategyFactory.Queue<string>();

            StreamSink<string> source1 = Stream.CreateSink<string>();
            StreamSink<string> results1 = Stream.CreateSink<string>();
            StreamSink<Exception> errors1 = Stream.CreateSink<Exception>();
            ControlledOperation<string, string> op1 = new();
            List<string> received1 = new();
            IListener l1 = results1.Listen(received1.Add);

            StreamSink<string> source2 = Stream.CreateSink<string>();
            StreamSink<string> results2 = Stream.CreateSink<string>();
            StreamSink<Exception> errors2 = Stream.CreateSink<Exception>();
            ControlledOperation<string, string> op2 = new();
            List<string> received2 = new();
            IListener l2 = results2.Listen(received2.Add);

            AsyncMapStatus<string> status1 = source1.MapAsyncImpl(
                results: results1,
                errors: errors1,
                operation: op1.Operation,
                strategy: sharedQueue,
                inputConverter: v => v,
                resultConverter: v => v);

            AsyncMapStatus<string> status2 = source2.MapAsyncImpl(
                results: results2,
                errors: errors2,
                operation: op2.Operation,
                strategy: sharedQueue,
                inputConverter: v => v,
                resultConverter: v => v);

            source1.Send("x");

            // Pipeline 2 must be able to start immediately despite pipeline 1's queue being busy —
            // proves each call gets its own independent scheduling state, per CreateState's contract.
            TestUtil.WaitUntil(() => op1.HasStarted("x"));
            source2.Send("y");
            TestUtil.WaitUntil(() => op2.HasStarted("y"));

            op1.Release("x", "X");
            op2.Release("y", "Y");
            TestUtil.WaitUntil(() => received1.Count == 1 && received2.Count == 1);

            status1.Dispose();
            status2.Dispose();
            l1.Unlisten();
            l2.Unlisten();
        }
    }
}
