using System;
using System.Threading;
using System.Threading.Tasks;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests.Internal
{
    [TestFixture]
    public class TransactionTests
    {
        [Test]
        public async Task PostSeeOutside()
        {
            OperationCanceledException actual = null;
            AutoResetEvent re = new AutoResetEvent(false);
            using (CancellationTokenSource cts = new CancellationTokenSource())
            {
                Task task = Task.Run(
                    () =>
                    {
                        Transaction.Post(
                            () =>
                            {
                                re.Set();

                                Thread.Sleep(5000);

                                cts.Token.ThrowIfCancellationRequested();
                            });
                    });

                re.WaitOne();

                cts.Cancel();

                try
                {
                    await task;
                }
                catch (OperationCanceledException e)
                {
                    actual = e;
                }
            }

            Assert.IsNotNull(actual);
        }

        [Test]
        public async Task PostSeeInside()
        {
            OperationCanceledException actual = null;
            AutoResetEvent re = new AutoResetEvent(false);
            using (CancellationTokenSource cts = new CancellationTokenSource())
            {
                Task task = Task.Run(
                    () =>
                    {
                        Transaction.Post(
                            () =>
                            {
                                re.Set();

                                Thread.Sleep(5000);

                                cts.Token.ThrowIfCancellationRequested();
                            });
                    });

                re.WaitOne();

                StreamSink<Unit> sink2 = Stream.CreateSink<Unit>();
                sink2.Listen(_ => cts.Cancel());
                sink2.Send(Unit.Value);

                try
                {
                    await task;
                }
                catch (OperationCanceledException e)
                {
                    actual = e;
                }
            }

            Assert.IsNotNull(actual);
        }
    }
}