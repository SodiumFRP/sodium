using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests
{
    [TestFixture]
    public class TransactionTests
    {
        [Test]
        public void RunConstruct()
        {
            List<int> @out = new List<int>();
            (StreamSink<int> s, IListener l) = Transaction.Run(() =>
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
            Task<IListener> t = Task.Run(() => Transaction.Run(() =>
            {
                Thread.Sleep(500);
                sink.Send(4);
                Stream<int> s = sink.Map(v => v * 2);
                IListener l2 = s.Listen(@out.Add);
                Thread.Sleep(500);
                return l2;
            }));
            await Task.Delay(250);
            sink.Send(5);
            await Task.Delay(500);
            sink.Send(6);
            IListener l = await t;
            sink.Send(7);
            l.Unlisten();
            sink.Send(8);

            CollectionAssert.AreEqual(new[] { 8, 12, 14 }, @out);
        }

        [Test]
        public async Task NestedRunConstruct()
        {
            List<int> @out = new List<int>();
            StreamSink<int> sink = Stream.CreateSink<int>();
            Task<IListener> t = Task.Run(() => Transaction.Run(() =>
            {
                Thread.Sleep(500);
                sink.Send(4);
                Stream<int> s = Transaction.Run(() => sink.Map(v => v * 2));
                IListener l2 = s.Listen(@out.Add);
                Thread.Sleep(500);
                return l2;
            }));
            await Task.Delay(250);
            sink.Send(5);
            await Task.Delay(500);
            sink.Send(6);
            IListener l = await t;
            sink.Send(7);
            l.Unlisten();
            sink.Send(8);

            CollectionAssert.AreEqual(new[] { 8, 12, 14 }, @out);
        }

        [Test]
        public void Post()
        {
            Cell<int> cell = Transaction.Run(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                return s.Hold(1);
            });
            int value = 0;
            Transaction.Post(() => value = cell.Sample());

            Assert.AreEqual(2, value);
        }

        [Test]
        public void NestedPost()
        {
            Cell<int> cell = Transaction.Run(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                Transaction.Post(() =>
                {
                    s.Send(3);
                    Transaction.Post(() => s.Send(5));
                });
                Transaction.Post(() => s.Send(4));
                return s.Hold(1);
            });

            Assert.AreEqual(5, cell.Sample());
        }

        [Test]
        public void PostInTransaction()
        {
            int value = 0;
            Transaction.RunVoid(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                Cell<int> c = s.Hold(1);
                Transaction.Post(() => value = c.Sample());
                Assert.AreEqual(0, value);
            });

            Assert.AreEqual(2, value);
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
                    Cell<int> c = s.Hold(1);
                    Transaction.Post(() => value = c.Sample());
                });
                Assert.AreEqual(0, value);
            });

            Assert.AreEqual(2, value);
        }

        [Test]
        public void PostInNestedTransaction2()
        {
            int value = 0;
            Transaction.RunVoid(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                Transaction.Run(() =>
                {
                    Cell<int> c = s.Hold(1);
                    Transaction.Post(() => value = c.Sample());
                    return Unit.Value;
                });
                Assert.AreEqual(0, value);
            });

            Assert.AreEqual(2, value);
        }

        [Test]
        public void PostInConstructTransaction()
        {
            int value = 0;
            Transaction.Run(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                Cell<int> c = s.Hold(1);
                Transaction.Post(() => value = c.Sample());
                Assert.AreEqual(0, value);
                return Unit.Value;
            });

            Assert.AreEqual(2, value);
        }

        [Test]
        public void PostInNestedConstructTransaction()
        {
            int value = 0;
            Transaction.Run(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                Transaction.RunVoid(() =>
                {
                    Cell<int> c = s.Hold(1);
                    Transaction.Post(() => value = c.Sample());
                });
                Assert.AreEqual(0, value);
                return Unit.Value;
            });

            Assert.AreEqual(2, value);
        }

        [Test]
        public void PostInNestedConstructTransaction2()
        {
            int value = 0;
            Transaction.Run(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                s.Send(2);
                Transaction.Run(() =>
                {
                    Cell<int> c = s.Hold(1);
                    Transaction.Post(() => value = c.Sample());
                    return Unit.Value;
                });
                Assert.AreEqual(0, value);
                return Unit.Value;
            });

            Assert.AreEqual(2, value);
        }

        [Test]
        public void IsActive()
        {
            bool isActive = Transaction.Run(Transaction.IsActive);

            Assert.IsTrue(isActive);
        }

        [Test]
        public void IsNotActive()
        {
            bool isActive = Transaction.IsActive();

            Assert.IsFalse(isActive);
        }

        [Test]
        public void IsNotActiveSeparateThread()
        {
            bool? threadIsActive1 = null;
            bool? threadIsActive2 = null;
            bool? threadIsActive3 = null;
            bool? threadIsActive4 = null;
            bool? threadIsActive5 = null;
            new Thread(() =>
            {
                threadIsActive1 = Transaction.IsActive();
                Thread.Sleep(500);
                threadIsActive2 = Transaction.IsActive();
                Transaction.RunVoid(() =>
                {
                    threadIsActive3 = Transaction.IsActive();
                    Thread.Sleep(500);
                    threadIsActive4 = Transaction.IsActive();
                });
                threadIsActive5 = Transaction.IsActive();
            }).Start();

            Thread.Sleep(250);
            bool isActive1 = Transaction.IsActive();
            Thread.Sleep(500);
            bool isActive2 = Transaction.IsActive();
            Thread.Sleep(500);
            bool isActive3 = Transaction.IsActive();

            Assert.IsFalse(isActive1);
            Assert.IsFalse(isActive2);
            Assert.IsFalse(isActive3);

            Assert.IsFalse(threadIsActive1);
            Assert.IsFalse(threadIsActive2);
            Assert.IsTrue(threadIsActive3);
            Assert.IsTrue(threadIsActive4);
            Assert.IsFalse(threadIsActive5);
        }
    }
}
