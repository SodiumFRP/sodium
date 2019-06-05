using System.Collections.Generic;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests
{
    [TestFixture]
    public class CommonTests
    {
        [Test]
        public void TestBaseSend1()
        {
            StreamSink<string> s = Stream.CreateSink<string>();
            List<string> @out = new List<string>();
            IListener l = s.Listen(@out.Add);
            s.Send("a");
            s.Send("b");
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "a", "b" }, @out);
        }

        [Test]
        public void TestOperationalSplit()
        {
            StreamSink<List<string>> a = Stream.CreateSink<List<string>>();
            Stream<string> b = Operational.Split<string, List<string>>(a);
            List<string> @out = new List<string>();
            IListener l = b.Listen(@out.Add);
            a.Send(new List<string> { "a", "b" });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "a", "b" }, @out);
        }

        [Test]
        public void TestOperationalDefer1()
        {
            StreamSink<string> a = Stream.CreateSink<string>();
            Stream<string> b = Operational.Defer(a);
            List<string> @out = new List<string>();
            IListener l = b.Listen(@out.Add);
            a.Send("a");
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "a" }, @out);
            List<string> out2 = new List<string>();
            IListener l2 = b.Listen(out2.Add);
            a.Send("b");
            l2.Unlisten();
            CollectionAssert.AreEqual(new[] { "b" }, out2);
        }

        [Test]
        public void TestOperationalDefer2()
        {
            StreamSink<string> a = Stream.CreateSink<string>();
            StreamSink<string> b = Stream.CreateSink<string>();
            Stream<string> c = Operational.Defer(a).OrElse(b);
            List<string> @out = new List<string>();
            IListener l = c.Listen(@out.Add);
            a.Send("a");
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "a" }, @out);
            List<string> out2 = new List<string>();
            IListener l2 = c.Listen(out2.Add);
            Transaction.RunVoid(() =>
            {
                a.Send("b");
                b.Send("B");
            });
            l2.Unlisten();
            CollectionAssert.AreEqual(new[] { "B", "b" }, out2);
        }

        [Test]
        public void TestStreamOrElse1()
        {
            StreamSink<int> a = Stream.CreateSink<int>();
            StreamSink<int> b = Stream.CreateSink<int>();
            Stream<int> c = a.OrElse(b);
            List<int> @out = new List<int>();
            IListener l = c.Listen(@out.Add);
            a.Send(0);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 0 }, @out);
            List<int> out2 = new List<int>();
            IListener l2 = c.Listen(out2.Add);
            b.Send(10);
            l2.Unlisten();
            CollectionAssert.AreEqual(new[] { 10 }, out2);
            List<int> out3 = new List<int>();
            IListener l3 = c.Listen(out3.Add);
            Transaction.RunVoid(() =>
            {
                a.Send(2);
                b.Send(20);
            });
            l3.Unlisten();
            CollectionAssert.AreEqual(new[] { 2 }, out3);
            List<int> out4 = new List<int>();
            IListener l4 = c.Listen(out4.Add);
            b.Send(30);
            l4.Unlisten();
            CollectionAssert.AreEqual(new[] { 30 }, out4);
        }

        [Test]
        public void TestOperationalDeferSimultaneous()
        {
            StreamSink<string> a = Stream.CreateSink<string>();
            StreamSink<string> b = Stream.CreateSink<string>();
            Stream<string> c = Operational.Defer(a).OrElse(Operational.Defer(b));
            List<string> @out = new List<string>();
            IListener l = c.Listen(@out.Add);
            b.Send("A");
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "A" }, @out);
            List<string> out2 = new List<string>();
            IListener l2 = c.Listen(out2.Add);
            Transaction.RunVoid(() =>
            {
                a.Send("b");
                b.Send("B");
            });
            l2.Unlisten();
            CollectionAssert.AreEqual(new[] { "b" }, out2);
        }

        [Test]
        public void TestUnitEqualsOperator_NonNull()
        {
            Unit u1 = Unit.Value;
            Unit u2 = Unit.Value;

            Assert.IsTrue(u1 == u2);
            Assert.IsTrue(u2 == u1);
        }

        [Test]
        public void TestUnitEqualsOperator_Null()
        {
            Unit u1 = null;
            Unit u2 = null;

            Assert.IsTrue(u1 == u2);
            Assert.IsTrue(u2 == u1);
        }

        [Test]
        public void TestUnitEqualsOperator_NonNullAndNull()
        {
            Unit u1 = Unit.Value;
            Unit u2 = null;

            Assert.IsFalse(u1 == u2);
            Assert.IsFalse(u2 == u1);
        }

        [Test]
        public void TestUnitNotEqualsOperator_NonNull()
        {
            Unit u1 = Unit.Value;
            Unit u2 = Unit.Value;

            Assert.IsFalse(u1 != u2);
            Assert.IsFalse(u2 != u1);
        }

        [Test]
        public void TestUnitNotEqualsOperator_Null()
        {
            Unit u1 = null;
            Unit u2 = null;

            Assert.IsFalse(u1 != u2);
            Assert.IsFalse(u2 != u1);
        }

        [Test]
        public void TestUnitNotEqualsOperator_NonNullAndNull()
        {
            Unit u1 = Unit.Value;
            Unit u2 = null;

            Assert.IsTrue(u1 != u2);
            Assert.IsTrue(u2 != u1);
        }
    }
}