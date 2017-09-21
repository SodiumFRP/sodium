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
            (StreamSink<int> s, IListener l) = Transaction.RunConstruct(() =>
            {
                StreamSink<int> sink = Stream.CreateSink<int>();
                sink.Send(4);
                Stream<int> sLocal = sink.Map(v => v * 2);
                IListener lLocal = sLocal.Listen(@out.Add);
                return (sink, lLocal);
            });
            s.Send(5);
            s.Send(6);
            s.Send(7);
            l.Unlisten();
            s.Send(8);

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

        [Test]
        public void Post()
        {
            DiscreteCell<int> cell = Transaction.Run(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                return s.Hold(1);
            });
            int value = 0;
            Transaction.Post(() => value = cell.Cell.Sample());

            Assert.AreEqual(value, 2);
        }

        [Test]
        public void PostInTransaction()
        {
            int value = 0;
            Transaction.RunVoid(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                DiscreteCell<int> c = s.Hold(1);
                Transaction.Post(() => value = c.Cell.Sample());
                Assert.AreEqual(value, 0);
            });

            Assert.AreEqual(value, 2);
        }

        [Test]
        public void PostInNestedTransaction()
        {
            int value = 0;
            Transaction.RunVoid(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                Transaction.RunVoid(() =>
                {
                    DiscreteCell<int> c = s.Hold(1);
                    Transaction.Post(() => value = c.Cell.Sample());
                });
                Assert.AreEqual(value, 0);
            });

            Assert.AreEqual(value, 2);
        }

        [Test]
        public void PostInNestedTransaction2()
        {
            int value = 0;
            Transaction.RunVoid(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                Transaction.RunConstruct(() =>
                {
                    DiscreteCell<int> c = s.Hold(1);
                    Transaction.Post(() => value = c.Cell.Sample());
                    return Unit.Value;
                });
                Assert.AreEqual(value, 0);
            });

            Assert.AreEqual(value, 2);
        }

        [Test]
        public void PostInConstructTransaction()
        {
            int value = 0;
            Transaction.RunConstruct(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                DiscreteCell<int> c = s.Hold(1);
                Transaction.Post(() => value = c.Cell.Sample());
                Assert.AreEqual(value, 0);
                return Unit.Value;
            });

            Assert.AreEqual(value, 2);
        }

        [Test]
        public void PostInNestedConstructTransaction()
        {
            int value = 0;
            Transaction.RunConstruct(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                Transaction.RunVoid(() =>
                {
                    DiscreteCell<int> c = s.Hold(1);
                    Transaction.Post(() => value = c.Cell.Sample());
                });
                Assert.AreEqual(value, 0);
                return Unit.Value;
            });

            Assert.AreEqual(value, 2);
        }

        [Test]
        public void PostInNestedConstructTransaction2()
        {
            int value = 0;
            Transaction.RunConstruct(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                Transaction.RunConstruct(() =>
                {
                    DiscreteCell<int> c = s.Hold(1);
                    Transaction.Post(() => value = c.Cell.Sample());
                    return Unit.Value;
                });
                Assert.AreEqual(value, 0);
                return Unit.Value;
            });

            Assert.AreEqual(value, 2);
        }
    }
}
