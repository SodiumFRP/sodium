using System;
using System.Collections.Generic;
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

            AsyncMapStatus<string> status = source.MapAsync(
                results: results,
                errors: errors,
                operation: op.Operation,
                strategy: AsyncConcurrencyStrategy.Parallel());

            source.Send("a");
            source.Send("b");

            TestUtil.WaitUntil(() => op.HasStarted("a") && op.HasStarted("b"));

            op.Release("b", "B");
            TestUtil.WaitUntil(() => received.Count == 1);
            op.Release("a", "A");
            TestUtil.WaitUntil(() => received.Count == 2);

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

            AsyncMapStatus<string> status = source.MapAsync(
                results: results,
                errors: errors,
                operation: op.Operation,
                strategy: AsyncConcurrencyStrategy.Queue());

            source.Send("a");
            source.Send("b");

            TestUtil.WaitUntil(() => op.HasStarted("a"));
            Assert.IsFalse(op.HasStarted("b"));

            op.Release("a", "A");
            TestUtil.WaitUntil(() => op.HasStarted("b"));
            op.Release("b", "B");

            TestUtil.WaitUntil(() => received.Count == 2);
            CollectionAssert.AreEqual(new[] { "A", "B" }, received);

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

            AsyncMapStatus<string> status = source.MapAsync(
                results: results,
                errors: errors,
                operation: op.Operation,
                strategy: strategy);

            source.Send("g1-a");
            source.Send("g1-b");
            source.Send("g2-a");

            TestUtil.WaitUntil(() => op.HasStarted("g1-a") && op.HasStarted("g2-a"));
            Assert.IsFalse(op.HasStarted("g1-b"));

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

            AsyncMapStatus<string> status = source.MapAsync(
                results: results,
                errors: errors,
                operation: op.Operation,
                strategy: AsyncConcurrencyStrategy.SwitchLatest());

            source.Send("a");
            TestUtil.WaitUntil(() => op.HasStarted("a"));
            source.Send("b");
            TestUtil.WaitUntil(() => op.HasStarted("b"));

            op.Release("a", "A");
            op.Release("b", "B");
            TestUtil.WaitUntil(() => received.Count == 1);

            System.Threading.Thread.Sleep(100);
            CollectionAssert.AreEqual(new[] { "B" }, received);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void Parallel_Queue_SwitchLatest_EachReturnTheSameCachedInstanceEveryCall()
        {
            // These are advertised as stateless and reusable; the wrapper caches one instance per
            // strategy rather than allocating fresh on every call.
            Assert.AreSame(AsyncConcurrencyStrategy.Parallel(), AsyncConcurrencyStrategy.Parallel());
            Assert.AreSame(AsyncConcurrencyStrategy.Queue(), AsyncConcurrencyStrategy.Queue());
            Assert.AreSame(AsyncConcurrencyStrategy.SwitchLatest(), AsyncConcurrencyStrategy.SwitchLatest());
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

            AsyncMapStatus<string> status = source.MapAsync(
                results: results,
                errors: errors,
                operation: (v, ct) => System.Threading.Tasks.Task.FromResult(Unit.Value),
                strategy: strategy);

            source.Send("a");
            source.Send("b");

            TestUtil.WaitUntil(() => received.Count == 2);
            Assert.AreEqual(2, strategy.AdmittedCount);

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
                System.Threading.Interlocked.Increment(ref this.count);
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
