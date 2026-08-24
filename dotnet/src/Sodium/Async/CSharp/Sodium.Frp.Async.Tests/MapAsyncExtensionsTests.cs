using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Async.Tests
{
    [TestFixture]
    public class MapAsyncExtensionsTests
    {
        [Test]
        public void MapAsync_UnitErasedStrategy_Overload()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<string> received = new();
            IListener l = results.Listen(received.Add);

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (v, _) => Task.FromResult(v.ToUpperInvariant()),
                    strategy: AsyncConcurrencyStrategy.Parallel());

            source.Send("hello");
            TestUtil.WaitUntil(() => received.Count == 1);
            Assert.AreEqual(expected: "HELLO", actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void MapAsync_TStrategyInputWithoutConverter_AcceptsTInputAsSubtypeOfTStrategyInput()
        {
            StreamSink<Dog> source = Stream.CreateSink<Dog>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<string> received = new();
            IListener l = results.Listen(received.Add);
            Dog dog = new();
            AlwaysStartStrategy<Animal, Unit> strategy = new();

            AsyncMapStatus<Dog> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (_, _) => Task.FromResult("done"),
                    strategy: strategy);

            source.Send(dog);
            TestUtil.WaitUntil(() => received.Count == 1);

            Assert.AreSame(expected: dog, actual: strategy.AdmittedValues[0]);
            Assert.AreEqual(expected: "done", actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void MapAsync_TStrategyInputWithConverter_AppliesInputConverter()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<string> received = new();
            IListener l = results.Listen(received.Add);
            AlwaysStartStrategy<int, Unit> strategy = new();

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (v, _) => Task.FromResult(v.ToUpperInvariant()),
                    strategy: strategy,
                    inputConverter: v => v.Length);

            source.Send("hello");
            TestUtil.WaitUntil(() => received.Count == 1);

            CollectionAssert.AreEqual(expected: new[] { 5 }, actual: strategy.AdmittedValues);
            Assert.AreEqual(expected: "HELLO", actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void MapAsync_TStrategyResultWithoutConverter_AcceptsTResultAsSubtypeOfTStrategyResult()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<Dog> results = Stream.CreateSink<Dog>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<Dog> received = new();
            IListener l = results.Listen(received.Add);
            Dog dog = new();

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (_, _) => Task.FromResult(dog),
                    strategy: new AlwaysStartStrategy<Unit, Animal>());

            source.Send("hello");
            TestUtil.WaitUntil(() => received.Count == 1);
            Assert.AreSame(expected: dog, actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void MapAsync_TStrategyResultWithConverter_AppliesResultConverter()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<string> received = new();
            IListener l = results.Listen(received.Add);
            AlwaysStartStrategy<Unit, int> strategy = new();

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (v, _) => Task.FromResult(v.ToUpperInvariant()),
                    strategy: strategy,
                    resultConverter: v => v.Length);

            source.Send("hello");
            TestUtil.WaitUntil(() => received.Count == 1);

            CollectionAssert.AreEqual(expected: new[] { 5 }, actual: strategy.CompletedResults);
            Assert.AreEqual(expected: "HELLO", actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void MapAsync_FourTypeArgsWithoutConverters_AcceptsBothAsSubtypes()
        {
            StreamSink<Dog> source = Stream.CreateSink<Dog>();
            StreamSink<Dog> results = Stream.CreateSink<Dog>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<Dog> received = new();
            IListener l = results.Listen(received.Add);
            Dog dog = new();

            AsyncMapStatus<Dog> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (v, _) => Task.FromResult(v),
                    strategy: new AlwaysStartStrategy<Animal, Animal>());

            source.Send(dog);
            TestUtil.WaitUntil(() => received.Count == 1);
            Assert.AreSame(expected: dog, actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void MapAsync_FourTypeArgsWithInputConverterOnly_AppliesInputConverter()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<Dog> results = Stream.CreateSink<Dog>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<Dog> received = new();
            IListener l = results.Listen(received.Add);
            Dog dog = new();
            AlwaysStartStrategy<int, Animal> strategy = new();

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (_, _) => Task.FromResult(dog),
                    strategy: strategy,
                    inputConverter: v => v.Length);

            source.Send("hello");
            TestUtil.WaitUntil(() => received.Count == 1);

            CollectionAssert.AreEqual(expected: new[] { 5 }, actual: strategy.AdmittedValues);
            Assert.AreSame(expected: dog, actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void MapAsync_FourTypeArgsWithResultConverterOnly_AppliesResultConverter()
        {
            StreamSink<Dog> source = Stream.CreateSink<Dog>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<string> received = new();
            IListener l = results.Listen(received.Add);
            Dog dog = new();
            AlwaysStartStrategy<Animal, int> strategy = new();

            AsyncMapStatus<Dog> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (_, _) => Task.FromResult("done"),
                    strategy: strategy,
                    resultConverter: v => v.Length);

            source.Send(dog);
            TestUtil.WaitUntil(() => received.Count == 1);

            CollectionAssert.AreEqual(expected: new[] { 4 }, actual: strategy.CompletedResults);
            Assert.AreEqual(expected: "done", actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void MapAsync_FullyGeneralOverload_AppliesBothConvertersToUnrelatedTypes()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<string> received = new();
            IListener l = results.Listen(received.Add);
            AlwaysStartStrategy<int, bool> strategy = new();

            // TStrategyInput (int, a length) and TStrategyResult (bool, "is long") are both
            // unrelated by inheritance to TInput/TResult (string) — only this overload permits it.
            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (v, _) => Task.FromResult(v.ToUpperInvariant()),
                    strategy: strategy,
                    inputConverter: v => v.Length,
                    resultConverter: v => v.Length > 3);

            source.Send("hello");
            TestUtil.WaitUntil(() => received.Count == 1);

            CollectionAssert.AreEqual(expected: new[] { 5 }, actual: strategy.AdmittedValues);
            CollectionAssert.AreEqual(expected: new[] { true }, actual: strategy.CompletedResults);
            Assert.AreEqual(expected: "HELLO", actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void CancelAll_CancelsEveryTrackedOperation()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            StreamSink<Unit> cancelAll = Stream.CreateSink<Unit>();
            ControlledOperation<string, string> op = new();
            List<string> received = new();
            IListener l = results.Listen(received.Add);

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: op.Operation,
                    strategy: AsyncConcurrencyStrategy.Parallel(),
                    cancelAll: cancelAll);

            source.Send("a");
            source.Send("b");
            TestUtil.WaitUntil(() => op.HasStarted("a") && op.HasStarted("b"));

            cancelAll.Send(Unit.Value);

            Thread.Sleep(200);

            Assert.AreEqual(
                expected: 0,
                actual: received.Count,
                message: "A canceled outcome must never be published.");

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void CancelMatching_CancelsOnlyTrackedOperationsForMatchingInputValues()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            StreamSink<IReadOnlyCollection<string>> cancelMatching = Stream.CreateSink<IReadOnlyCollection<string>>();
            ControlledOperation<string, string> op = new();
            List<string> received = new();
            IListener l = results.Listen(received.Add);

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: op.Operation,
                    strategy: AsyncConcurrencyStrategy.Parallel(),
                    cancelMatching: cancelMatching);

            source.Send("a");
            source.Send("b");
            TestUtil.WaitUntil(() => op.HasStarted("a") && op.HasStarted("b"));

            cancelMatching.Send(new[] { "a" });

            op.Release(input: "b", result: "B");
            TestUtil.WaitUntil(() => received.Count == 1);

            Thread.Sleep(100);
            CollectionAssert.AreEqual(expected: new[] { "B" }, actual: received);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void CancelOnDisposeTrue_CancelsInFlightItem()
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
                    strategy: AsyncConcurrencyStrategy.Parallel(),
                    cancelOnDispose: true);

            source.Send("a");
            TestUtil.WaitUntil(() => op.HasStarted("a"));

            status.Dispose();

            Thread.Sleep(200);
            Assert.AreEqual(expected: 0, actual: received.Count);

            l.Unlisten();
        }

        [Test]
        public void CancelOnDisposeFalse_LetsInFlightItemFinishAndPublish()
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
                    strategy: AsyncConcurrencyStrategy.Parallel(),
                    cancelOnDispose: false);

            source.Send("a");
            TestUtil.WaitUntil(() => op.HasStarted("a"));

            status.Dispose();
            op.Release(input: "a", result: "A");

            TestUtil.WaitUntil(() => received.Count == 1);
            CollectionAssert.AreEqual(expected: new[] { "A" }, actual: received);

            l.Unlisten();
        }

        [Test]
        public void FailedOperationPublishesToErrors()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            InvalidOperationException thrown = new("boom");
            List<Exception> received = new();
            IListener l = errors.Listen(received.Add);

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: (_, _) => Task.FromException<string>(thrown),
                    strategy: AsyncConcurrencyStrategy.Parallel());

            source.Send("hello");
            TestUtil.WaitUntil(() => received.Count == 1);
            Assert.AreSame(expected: thrown, actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void ItemsAndIsRunning_ReflectQueuedAndRunningStatus()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            ControlledOperation<string, string> op = new();

            AsyncMapStatus<string> status =
                source.MapAsync(
                    results: results,
                    errors: errors,
                    operation: op.Operation,
                    strategy: AsyncConcurrencyStrategy.Queue());

            Assert.IsFalse(status.IsRunning.Sample());
            Assert.AreEqual(expected: 0, actual: status.Items.Sample().Count);

            source.Send("a");
            source.Send("b");
            TestUtil.WaitUntil(() => op.HasStarted("a"));
            TestUtil.WaitUntil(() => status.IsRunning.Sample());

            IReadOnlyList<AsyncItem<string>> items = status.Items.Sample();
            Assert.AreEqual(expected: 2, actual: items.Count);

            op.Release(input: "a", result: "A");
            TestUtil.WaitUntil(() => op.HasStarted("b"));
            op.Release(input: "b", result: "B");

            TestUtil.WaitUntil(() => status.Items.Sample().Count == 0);
            Assert.IsFalse(status.IsRunning.Sample());

            status.Dispose();
        }

        private class Animal
        {
        }

        private sealed class Dog : Animal
        {
        }

        /// <summary>
        ///     Starts everything immediately, like the built-in Parallel, but works against arbitrary
        ///     TStrategyInput/TStrategyResult and records both what it was admitted with and what it
        ///     saw on completion — so a test can assert a converter actually ran, not merely compiled.
        /// </summary>
        private sealed class AlwaysStartStrategy<TStrategyInput, TStrategyResult>
            : AsyncConcurrencyStrategy<TStrategyInput, TStrategyResult, Unit>
        {
            public readonly List<TStrategyInput> AdmittedValues = new();
            public readonly List<TStrategyResult> CompletedResults = new();

            protected override Unit CreateState() => Unit.Value;

            protected override IReadOnlyList<AsyncToStart<TStrategyInput>> Admit(
                Unit state,
                AsyncQueuedItem<TStrategyInput> incoming)
            {
                lock (this.AdmittedValues)
                {
                    this.AdmittedValues.Add(incoming.Value);
                }

                return new[] { new AsyncToStart<TStrategyInput>(incoming) };
            }

            protected override AsyncStrategyResult<TStrategyInput> OnCompleted(
                Unit state,
                AsyncQueuedItem<TStrategyInput> item,
                AsyncOutcome<TStrategyResult> outcome)
            {
                outcome.MatchVoid(
                    onSucceeded: v =>
                    {
                        lock (this.CompletedResults)
                        {
                            this.CompletedResults.Add(v);
                        }
                    },
                    onFailed: null,
                    onCanceled: null);

                return new AsyncStrategyResult<TStrategyInput>(
                    publish: true,
                    next: AsyncStrategyResult<TStrategyInput>.None);
            }
        }
    }
}