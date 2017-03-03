using System.Collections.Generic;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class CommonTests
    {
        [Test]
        public void TestBaseSend1()
        {
            StreamSink<string> s = new StreamSink<string>();
            List<string> @out = new List<string>();
            using (s.Listen(@out.Add))
            {
                s.Send("a");
                s.Send("b");
            }
            CollectionAssert.AreEqual(new[] { "a", "b" }, @out);
        }

        [Test]
        public void TestOperationalSplit()
        {
            StreamSink<List<string>> a = new StreamSink<List<string>>();
            Stream<string> b = Operational.Split<string, List<string>>(a);
            List<string> @out = new List<string>();
            using (b.Listen(@out.Add))
            {
                a.Send(new List<string> { "a", "b" });
            }
            CollectionAssert.AreEqual(new[] { "a", "b" }, @out);
        }

        [Test]
        public void TestOperationalDefer1()
        {
            StreamSink<string> a = new StreamSink<string>();
            Stream<string> b = Operational.Defer(a);
            List<string> @out = new List<string>();
            using (b.Listen(@out.Add))
            {
                a.Send("a");
            }
            CollectionAssert.AreEqual(new[] { "a" }, @out);
            List<string> out2 = new List<string>();
            using (b.Listen(out2.Add))
            {
                a.Send("b");
            }
            CollectionAssert.AreEqual(new[] { "b" }, out2);
        }

        [Test]
        public void TestOperationalDefer2()
        {
            StreamSink<string> a = new StreamSink<string>();
            StreamSink<string> b = new StreamSink<string>();
            Stream<string> c = Operational.Defer(a).OrElse(b);
            List<string> @out = new List<string>();
            using (c.Listen(@out.Add))
            {
                a.Send("a");
            }
            CollectionAssert.AreEqual(new[] { "a" }, @out);
            List<string> out2 = new List<string>();
            using (c.Listen(out2.Add))
            {
                Transaction.RunVoid(() =>
                {
                    a.Send("b");
                    b.Send("B");
                });
            }
            CollectionAssert.AreEqual(new[] { "B", "b" }, out2);
        }

        [Test]
        public void TestStreamOrElse1()
        {
            StreamSink<int> a = new StreamSink<int>();
            StreamSink<int> b = new StreamSink<int>();
            Stream<int> c = a.OrElse(b);
            List<int> @out = new List<int>();
            using (c.Listen(@out.Add))
            {
                a.Send(0);
            }
            CollectionAssert.AreEqual(new[] { 0 }, @out);
            List<int> out2 = new List<int>();
            using (c.Listen(out2.Add))
            {
                b.Send(10);
            }
            CollectionAssert.AreEqual(new[] { 10 }, out2);
            List<int> out3 = new List<int>();
            using (c.Listen(out3.Add))
            {
                Transaction.RunVoid(() =>
                {
                    a.Send(2);
                    b.Send(20);
                });
            }
            CollectionAssert.AreEqual(new[] { 2 }, out3);
            List<int> out4 = new List<int>();
            using (c.Listen(out4.Add))
            {
                b.Send(30);
            }
            CollectionAssert.AreEqual(new[] { 30 }, out4);
        }

        [Test]
        public void TestOperationalDeferSimultaneous()
        {
            StreamSink<string> a = new StreamSink<string>();
            StreamSink<string> b = new StreamSink<string>();
            Stream<string> c = Operational.Defer(a).OrElse(Operational.Defer(b));
            List<string> @out = new List<string>();
            using (c.Listen(@out.Add))
            {
                b.Send("A");
            }
            CollectionAssert.AreEqual(new[] { "A" }, @out);
            List<string> out2 = new List<string>();
            using (c.Listen(out2.Add))
            {
                Transaction.RunVoid(() =>
                {
                    a.Send("b");
                    b.Send("B");
                });
            }
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