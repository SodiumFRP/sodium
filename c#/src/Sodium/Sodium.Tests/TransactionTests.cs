using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class TransactionTests
    {
        [Test]
        public void RunConstruct()
        {
            List<int> @out = new List<int>();
            Tuple<StreamSink<int>, IListener> t = Transaction.RunConstruct(() =>
            {
                StreamSink<int> sink = Stream.CreateSink<int>();
                sink.Send(4);
                Stream<int> s = sink.Map(v => v * 2);
                IListener l = s.Listen(@out.Add);
                return Tuple.Create(sink, l);
            });
            t.Item1.Send(5);
            t.Item1.Send(6);
            t.Item1.Send(7);
            t.Item2.Unlisten();
            t.Item1.Send(8);

            CollectionAssert.AreEqual(new[] { 8, 10, 12, 14 }, @out);
        }

        [Test]
        public async Task RunConstructIgnoresOtherTransactions()
        {
            List<int> @out = new List<int>();
            StreamSink<int> sink = Stream.CreateSink<int>();
            Task<IListener> t = Task.Run(() => Transaction.RunConstruct(() =>
            {
                Thread.Sleep(500);
                sink.Send(4);
                Stream<int> s = sink.Map(v => v * 2);
                IListener l2 = s.Listen(@out.Add);
                Thread.Sleep(500);
                return l2;
            }));
            sink.Send(5);
            await Task.Delay(750);
            sink.Send(6);
            IListener l = await t;
            sink.Send(7);
            l.Unlisten();
            sink.Send(8);

            CollectionAssert.AreEqual(new[] { 8, 14 }, @out);
        }

        [Test]
        public async Task NestedRunConstruct()
        {
            List<int> @out = new List<int>();
            StreamSink<int> sink = Stream.CreateSink<int>();
            Task<IListener> t = Task.Run(() => Transaction.RunConstruct(() =>
            {
                Thread.Sleep(500);
                sink.Send(4);
                //Stream<int> s = Transaction.RunConstruct(() => sink.Map(v => v * 2));
                Stream<int> s = sink.Map(v => v * 2);
                IListener l2 = s.Listen(@out.Add);
                Thread.Sleep(500);
                return l2;
            }));
            sink.Send(5);
            await Task.Delay(750);
            sink.Send(6);
            IListener l = await t;
            sink.Send(7);
            l.Unlisten();
            sink.Send(8);

            CollectionAssert.AreEqual(new[] { 8, 14 }, @out);
        }
    }
}
