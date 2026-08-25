using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using NUnit.Framework;

namespace Sodium.Frp.Async.Tests
{
    [TestFixture]
    public class MapAsyncImplTests
    {
        [Test]
        public void SuccessfulOperationPublishesToResults()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<string> received = new();
            IListener l = results.Listen(received.Add);

            AsyncMapStatus<string> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: (v, _) => Task.FromResult(v.ToUpperInvariant()),
                strategy: AsyncConcurrencyStrategyFactory.Parallel("unused"),
                inputConverter: v => v,
                resultConverter: v => v);

            source.Send("hello");

            TestUtil.WaitUntil(() => received.Count == 1);
            Assert.AreEqual(expected: "HELLO", actual: received[0]);

            status.Dispose();
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

            AsyncMapStatus<string> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: (_, _) => Task.FromException<string>(thrown),
                strategy: AsyncConcurrencyStrategyFactory.Parallel("unused"),
                inputConverter: v => v,
                resultConverter: v => v);

            source.Send("hello");

            TestUtil.WaitUntil(() => received.Count == 1);
            Assert.AreSame(expected: thrown, actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void InputAndResultConvertersAreAppliedBeforeTheStrategySeesThem()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            RecordingStrategy<int, int> strategy = new();
            List<string> received = new();
            IListener l = results.Listen(received.Add);

            // TStrategyInput/TStrategyResult (int, a length) are unrelated by inheritance to
            // TInput/TResult (string) — exactly the case only this fully general overload permits.
            AsyncMapStatus<string> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: (v, _) => Task.FromResult(v.ToUpperInvariant()),
                strategy: strategy,
                inputConverter: v => v.Length,
                resultConverter: v => v.Length);

            source.Send("hello");

            TestUtil.WaitUntil(() => received.Count == 1);

            // The strategy only ever sees the converted int, never the original string.
            CollectionAssert.AreEqual(expected: new[] { 5 }, actual: strategy.AdmittedValues);

            // Meanwhile the real TResult published is the untouched, unconverted operation output.
            Assert.AreEqual(expected: "HELLO", actual: received[0]);

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void CustomStrategyCanRejectAnIncomingValueOutright()
        {
            StreamSink<int> source = Stream.CreateSink<int>();
            StreamSink<int> results = Stream.CreateSink<int>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            RejectNegativeStrategy strategy = new();
            List<int> received = new();
            IListener l = results.Listen(received.Add);

            AsyncMapStatus<int> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: (v, _) => Task.FromResult(v),
                strategy: strategy,
                inputConverter: v => v,
                resultConverter: v => v);

            source.Send(-1);
            source.Send(2);

            TestUtil.WaitUntil(() => received.Count == 1);

            // -1 was rejected by the strategy — canceled and left permanently Queued, per the
            // documented "reject outright" idiom — and so never reached the operation; only the
            // non-negative value made it through.
            CollectionAssert.AreEqual(expected: new[] { 2 }, actual: received);

            // The rejected item is still visible, forever Queued — that's the visible cost of this
            // idiom, called out in AsyncConcurrencyStrategy's own remarks.
            Assert.IsTrue(
                status.Items.Sample().Any(i => i is { Value: -1, Status: AsyncItemStatus.Queued }));

            status.Dispose();
            l.Unlisten();
        }

        /// <summary>
        ///     Regression test for a real bug found while writing
        ///     <see cref="CustomStrategyCanRejectAnIncomingValueOutright" />: a strategy is free to
        ///     call <see cref="AsyncMapBase.AsyncQueuedItem{TInput}.Cancel" /> on <c>incoming</c> and still return
        ///     it as an <see cref="AsyncMapBase.AsyncToStart{TInput}" /> in the same <c>Admit</c> call — nothing in
        ///     that method's contract forbids it, unlike the "reject outright" idiom above, which
        ///     cancels but deliberately never promotes. Doing so currently crashes:
        ///     <c>PromoteAndLaunch</c>'s "already canceled" branch calls <c>Complete</c>
        ///     synchronously, inline, still inside the transaction that's processing the original
        ///     admission — where the normal-start branch just below it defers through
        ///     <c>TransactionInternal.PostImpl</c> specifically to avoid this. <c>Complete</c> then
        ///     opens its own transaction via <c>TransactionInternal.RunImpl</c>, which is illegal
        ///     while one is already open: the <c>Send</c> inside it throws
        ///     <c>InvalidOperationException("Send may not be called inside a callback.")</c>.
        ///     Expected to fail until <c>PromoteAndLaunch</c> defers that branch the same way.
        /// </summary>
        [Test]
        public void Admit_CancelingAndPromotingTheSameItemInOneCall_CompletesItAsCanceledInstead()
        {
            StreamSink<int> source = Stream.CreateSink<int>();
            StreamSink<int> results = Stream.CreateSink<int>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<int> received = new();
            IListener l = results.Listen(received.Add);

            AsyncMapStatus<int> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: (v, _) => Task.FromResult(v),
                strategy: new CancelAndPromoteSameItemStrategy(),
                inputConverter: v => v,
                resultConverter: v => v);

            Assert.DoesNotThrow(
                code: () => source.Send(1),
                message: "Canceling and promoting the same item in one Admit call should complete it as " +
                         "Canceled, not crash the transaction that admitted it.");

            Thread.Sleep(100);
            Assert.AreEqual(expected: 0, actual: received.Count, message: "A canceled outcome must never be published.");

            status.Dispose();
            l.Unlisten();
        }

        [Test]
        public void Dispose_StopsFurtherAdmission()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            List<string> received = new();
            IListener l = results.Listen(received.Add);

            AsyncMapStatus<string> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: (v, _) => Task.FromResult(v),
                strategy: AsyncConcurrencyStrategyFactory.Parallel("unused"),
                inputConverter: v => v,
                resultConverter: v => v);

            status.Dispose();
            source.Send("after-dispose");

            Thread.Sleep(100);
            Assert.AreEqual(expected: 0, actual: received.Count);

            l.Unlisten();
        }

        [Test]
        public void Dispose_WithCancelOnDisposeTrue_CancelsInFlightItem()
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
                resultConverter: v => v,
                cancelOnDispose: true);

            source.Send("a");
            TestUtil.WaitUntil(() => op.HasStarted("a"));

            status.Dispose();

            Thread.Sleep(200);
            Assert.AreEqual(expected: 0, actual: received.Count, message: "A canceled outcome must never be published.");

            l.Unlisten();
        }

        [Test]
        public void Dispose_WithCancelOnDisposeFalse_LetsInFlightItemFinishAndPublish()
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
                resultConverter: v => v,
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
        public void ItemsAndIsRunning_ReflectQueuedAndRunningStatus()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();
            ControlledOperation<string, string> op = new();

            AsyncMapStatus<string> status = source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: op.Operation,
                strategy: AsyncConcurrencyStrategyFactory.Queue<string>(),
                inputConverter: v => v,
                resultConverter: v => v);

            Assert.IsFalse(status.IsRunning.Sample());
            Assert.AreEqual(expected: 0, actual: status.Items.Sample().Count);

            source.Send("a");
            source.Send("b");

            TestUtil.WaitUntil(() => op.HasStarted("a"));
            TestUtil.WaitUntil(() => status.IsRunning.Sample());

            IReadOnlyList<AsyncItem<string>> items = status.Items.Sample();
            Assert.AreEqual(expected: 2, actual: items.Count);
            Assert.IsTrue(items.Any(i => i is { Value: "a", Status: AsyncItemStatus.Running }));
            Assert.IsTrue(items.Any(i => i is { Value: "b", Status: AsyncItemStatus.Queued }));

            op.Release(input: "a", result: "A");
            TestUtil.WaitUntil(() => op.HasStarted("b"));

            op.Release(input: "b", result: "B");
            TestUtil.WaitUntil(() => status.Items.Sample().Count == 0);
            Assert.IsFalse(status.IsRunning.Sample());

            status.Dispose();
        }

        [Test]
        public void NullSourceThrowsArgumentNullException()
        {
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();

            Assert.Throws<ArgumentNullException>(() =>
                AsyncStreamUtility.MapAsyncImpl<string, string, string, string>(
                    source: null!,
                    results: results,
                    errors: errors,
                    operation: (v, _) => Task.FromResult(v),
                    strategy: AsyncConcurrencyStrategyFactory.Parallel("unused"),
                    inputConverter: v => v,
                    resultConverter: v => v));
        }

        [Test]
        public void NullResultsThrowsArgumentNullException()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();

            Assert.Throws<ArgumentNullException>(() =>
                source.MapAsyncImpl(
                    results: null!,
                    errors: errors,
                    operation: (v, _) => Task.FromResult(v),
                    strategy: AsyncConcurrencyStrategyFactory.Parallel("unused"),
                    inputConverter: v => v,
                    resultConverter: v => v));
        }

        [Test]
        public void NullErrorsThrowsArgumentNullException()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();

            Assert.Throws<ArgumentNullException>(() =>
                source.MapAsyncImpl(
                    results: results,
                    errors: null!,
                    operation: (v, _) => Task.FromResult(v),
                    strategy: AsyncConcurrencyStrategyFactory.Parallel("unused"),
                    inputConverter: v => v,
                    resultConverter: v => v));
        }

        [Test]
        public void NullOperationThrowsArgumentNullException()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();

            Assert.Throws<ArgumentNullException>(() =>
                source.MapAsyncImpl(
                    results: results,
                    errors: errors,
                    operation: null!,
                    strategy: AsyncConcurrencyStrategyFactory.Parallel("unused"),
                    inputConverter: v => v,
                    resultConverter: v => v));
        }

        [Test]
        public void NullStrategyThrowsArgumentNullException()
        {
            StreamSink<string> source = Stream.CreateSink<string>();
            StreamSink<string> results = Stream.CreateSink<string>();
            StreamSink<Exception> errors = Stream.CreateSink<Exception>();

            Assert.Throws<ArgumentNullException>(() =>
                source.MapAsyncImpl(
                    results: results,
                    errors: errors,
                    operation: (v, _) => Task.FromResult(v),
                    strategy: null!,
                    inputConverter: v => v,
                    resultConverter: v => v));
        }

        /// <summary>Starts everything immediately and records the converted value each item was admitted with.</summary>
        private sealed class RecordingStrategy<TStrategyInput, TStrategyResult>
            : AsyncConcurrencyStrategy<TStrategyInput, TStrategyResult, object?>
        {
            // This state is global and not per MapAsync call.  Per MapAsync state needs to be held in a TState object.
            private readonly List<TStrategyInput> admittedValues = new();

            public IReadOnlyList<TStrategyInput> AdmittedValues => this.admittedValues;

            protected override object? CreateState() => null;

            protected internal override IReadOnlyList<AsyncToStart<TStrategyInput>> Admit(
                object? state,
                AsyncQueuedItem<TStrategyInput> incoming)
            {
                this.admittedValues.Add(incoming.Value);

                return new[] { new AsyncToStart<TStrategyInput>(incoming) };
            }

            protected internal override AsyncStrategyResult<TStrategyInput> OnCompleted(
                object? state,
                AsyncQueuedItem<TStrategyInput> item,
                AsyncOutcome<TStrategyResult> outcome) =>
                new(publish: true, next: AsyncStrategyResult<TStrategyInput>.None);
        }

        /// <summary>
        ///     Rejects a negative value outright: canceled the instant it's admitted, and never
        ///     promoted — the "leave it permanently Queued" idiom the base class documents for
        ///     outright rejection, rather than canceling and also returning it as an
        ///     <see cref="AsyncMapBase.AsyncToStart{TInput}" /> to start in the same call.
        /// </summary>
        private sealed class RejectNegativeStrategy : AsyncConcurrencyStrategy<int, int, object?>
        {
            protected override object? CreateState() => null;

            protected internal override IReadOnlyList<AsyncToStart<int>> Admit(
                object? state,
                AsyncQueuedItem<int> incoming)
            {
                if (incoming.Value < 0)
                {
                    incoming.Cancel();
                    return AsyncStrategyResult<int>.None;
                }

                return new[] { new AsyncToStart<int>(incoming) };
            }

            protected internal override AsyncStrategyResult<int> OnCompleted(
                object? state,
                AsyncQueuedItem<int> item,
                AsyncOutcome<int> outcome) =>
                new(publish: true, next: AsyncStrategyResult<int>.None);
        }

        /// <summary>
        ///     Cancels every incoming value and, unlike <see cref="RejectNegativeStrategy" />, still
        ///     returns it as an <see cref="AsyncMapBase.AsyncToStart{TInput}" /> to promote in the same call —
        ///     exercising <c>PromoteAndLaunch</c>'s "already canceled while queued" branch
        ///     synchronously, from within the same transaction as the admission itself, rather than
        ///     from a later transaction the way that branch is normally reached. (An external
        ///     cancelAll/cancelMatching firing, or another item's completion promoting a
        ///     previously-queued one.)
        /// </summary>
        private sealed class CancelAndPromoteSameItemStrategy : AsyncConcurrencyStrategy<int, int, object?>
        {
            protected override object? CreateState() => null;

            protected internal override IReadOnlyList<AsyncToStart<int>> Admit(
                object? state,
                AsyncQueuedItem<int> incoming)
            {
                incoming.Cancel();
                return new[] { new AsyncToStart<int>(incoming) };
            }

            protected internal override AsyncStrategyResult<int> OnCompleted(
                object? state,
                AsyncQueuedItem<int> item,
                AsyncOutcome<int> outcome) =>
                new(publish: true, next: AsyncStrategyResult<int>.None);
        }
    }
}
