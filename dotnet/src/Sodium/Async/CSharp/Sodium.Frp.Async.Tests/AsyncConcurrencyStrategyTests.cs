using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Async.Tests
{
    [TestFixture]
    public class AsyncConcurrencyStrategyTests
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

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: op.Operation,
                    strategy: AsyncConcurrencyStrategy.Parallel());

            source.Send("a");
            source.Send("b");

            TestUtil.WaitUntil(() => op.HasStarted("a") && op.HasStarted("b"));

            op.Release(input: "b", result: "B");
            TestUtil.WaitUntil(() => received.Count == 1);
            op.Release(input: "a", result: "A");
            TestUtil.WaitUntil(() => received.Count == 2);

            CollectionAssert.AreEqual(expected: new[] { "B", "A" }, actual: received);

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

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: op.Operation,
                    strategy: AsyncConcurrencyStrategy.Queue());

            source.Send("a");
            source.Send("b");

            TestUtil.WaitUntil(() => op.HasStarted("a"));
            Assert.IsFalse(op.HasStarted("b"));

            op.Release(input: "a", result: "A");
            TestUtil.WaitUntil(() => op.HasStarted("b"));
            op.Release(input: "b", result: "B");

            TestUtil.WaitUntil(() => received.Count == 2);
            CollectionAssert.AreEqual(expected: new[] { "A", "B" }, actual: received);

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

            AsyncConcurrencyStrategyBase<string, Unit> strategy =
                AsyncConcurrencyStrategy.QueuePerGroup<string>().Create(v => v.Split('-')[0]);

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: op.Operation,
                    strategy: strategy);

            source.Send("g1-a");
            source.Send("g1-b");
            source.Send("g2-a");

            TestUtil.WaitUntil(() => op.HasStarted("g1-a") && op.HasStarted("g2-a"));
            Assert.IsFalse(op.HasStarted("g1-b"));

            op.Release(input: "g1-a", result: "A1");
            TestUtil.WaitUntil(() => op.HasStarted("g1-b"));

            op.Release(input: "g1-b", result: "B1");
            op.Release(input: "g2-a", result: "A2");
            TestUtil.WaitUntil(() => received.Count == 3);

            CollectionAssert.AreEquivalent(expected: new[] { "A1", "B1", "A2" }, actual: received);

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

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: op.Operation,
                    strategy: AsyncConcurrencyStrategy.SwitchLatest());

            source.Send("a");
            TestUtil.WaitUntil(() => op.HasStarted("a"));
            source.Send("b");
            TestUtil.WaitUntil(() => op.HasStarted("b"));

            op.Release(input: "a", result: "A");
            op.Release(input: "b", result: "B");
            TestUtil.WaitUntil(() => received.Count == 1);

            Thread.Sleep(100);
            CollectionAssert.AreEqual(expected: new[] { "B" }, actual: received);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void Parallel_Queue_SwitchLatest_EachReturnTheSameCachedInstanceEveryCall()
        {
            // These are advertised as stateless and reusable; the wrapper caches one instance per
            // strategy rather than allocating fresh on every call.
            Assert.AreSame(expected: AsyncConcurrencyStrategy.Parallel(), actual: AsyncConcurrencyStrategy.Parallel());
            Assert.AreSame(expected: AsyncConcurrencyStrategy.Queue(), actual: AsyncConcurrencyStrategy.Queue());

            Assert.AreSame(
                expected: AsyncConcurrencyStrategy.SwitchLatest(),
                actual: AsyncConcurrencyStrategy.SwitchLatest());
        }

        [Test]
        public void CustomStrategy_ViaUnitShorthandBase_Works()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<Unit> results = Stream.CreateSink<Unit>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<Unit> received = new();
            IListener l = results.Listen(received.Add);

            CountingStrategy strategy = new();

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (_, _) => Task.FromResult(Unit.Value),
                    strategy: strategy);

            source.Send("a");
            source.Send("b");

            TestUtil.WaitUntil(() => received.Count == 2);
            Assert.AreEqual(expected: 2, actual: strategy.AdmittedCount);

            status.Dispose();
            l.Unlisten();
        }

        /// <summary>
        ///     A trivial custom strategy against the <see cref="AsyncConcurrencyStrategy{TState}" />
        ///     shorthand (input and result both fixed to <see cref="Unit" />) — every value starts
        ///     immediately, like Parallel, but also counts admissions.
        /// </summary>
        private sealed class CountingStrategy : AsyncConcurrencyStrategy<int>
        {
            private int count;

            public int AdmittedCount => this.count;

            protected override int CreateState() => 0;

            protected override IReadOnlyList<AsyncToStart<Unit>> Admit(
                int state,
                AsyncQueuedItem<Unit> incoming)
            {
                Interlocked.Increment(ref this.count);
                return new[] { new AsyncToStart<Unit>(incoming) };
            }

            protected override AsyncStrategyResult<Unit> OnCompleted(
                int state,
                AsyncQueuedItem<Unit> item,
                AsyncOutcome<Unit> outcome) =>
                new(publish: true, next: AsyncStrategyResult<Unit>.None);
        }
    }
}