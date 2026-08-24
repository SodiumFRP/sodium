using System;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using JetBrains.Annotations;

namespace Sodium.Frp.Async.Tests
{
    internal static class TestUtil
    {
        /// <summary>Polls <paramref name="condition" /> until it's true, or fails the test via timeout.</summary>
        public static void WaitUntil([InstantHandle] Func<bool> condition, int timeoutMs = 5000)
        {
            Stopwatch stopwatch = Stopwatch.StartNew();

            while (!condition())
            {
                if (stopwatch.ElapsedMilliseconds > timeoutMs)
                {
                    throw new TimeoutException("Condition was not met within the timeout.");
                }

                Thread.Sleep(10);
            }
        }
    }

    /// <summary>
    ///     An async operation, keyed by input, that a test controls the completion of explicitly via
    ///     <see cref="Release" />/<see cref="Fail" /> rather than racing real time. Also records which
    ///     inputs have actually been invoked, so a test can assert an operation started running
    ///     (rather than merely being admitted) before releasing it — the distinction that makes Queue
    ///     vs Parallel observable.
    /// </summary>
    internal sealed class ControlledOperation<TInput, TResult>
        where TInput : notnull
    {
        private readonly ConcurrentDictionary<TInput, TaskCompletionSource<TResult>> gates = new();
        private readonly ConcurrentDictionary<TInput, bool> startedInputs = new();

        public Func<TInput, CancellationToken, Task<TResult>> Operation => this.Run;

        public bool HasStarted(TInput input) => this.startedInputs.ContainsKey(input);

        public void Release(TInput input, TResult result) => this.GateFor(input).TrySetResult(result);

        public void Fail(TInput input, Exception error) => this.GateFor(input).TrySetException(error);

        private async Task<TResult> Run(TInput input, CancellationToken token)
        {
            this.startedInputs[input] = true;

            TaskCompletionSource<TResult> tcs = this.GateFor(input);

            using CancellationTokenRegistration registration =
                token.Register(() => tcs.TrySetCanceled(token));

            return await tcs.Task.ConfigureAwait(false);
        }

        private TaskCompletionSource<TResult> GateFor(TInput input) =>
            this.gates.GetOrAdd(input, _ => new TaskCompletionSource<TResult>());
    }
}
