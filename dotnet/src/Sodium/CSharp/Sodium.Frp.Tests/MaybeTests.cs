using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests
{
    [TestFixture]
    public class MaybeTests
    {
        [Test]
        public void DefaultConstructorTest()
        {
            Maybe<int> m = new Maybe<int>();

            Assert.IsFalse(m.HasValue());
        }

        [Test]
        public void DefaultValueTest()
        {
            Maybe<int> m = default(Maybe<int>);

            Assert.IsFalse(m.HasValue());
        }

        [Test]
        public void TestSome()
        {
            Maybe<int> m = Maybe.Some(2);

            int n = m.Match(v => v, () => 0);

            Assert.AreEqual(2, n);
        }

        [Test]
        public void TestNone()
        {
            Maybe<int> m = Maybe.None;

            int n = m.Match(v => 0, () => 1);

            Assert.AreEqual(1, n);
        }

        [Test]
        public void EqualityTest()
        {
            Maybe<int> m1 = Maybe.Some(2);
            Maybe<int> m2 = Maybe.Some(2);

            Assert.AreEqual(m1, m2);
        }

        [Test]
        public void EqualityTestNone()
        {
            Maybe<int> m1 = Maybe.None;
            Maybe<int> m2 = Maybe.None;

            Assert.AreEqual(m1, m2);
        }

        [Test]
        public void NonEqualityTest1()
        {
            Maybe<int> m1 = Maybe.Some(2);
            Maybe<int> m2 = Maybe.None;

            Assert.AreNotEqual(m1, m2);
        }

        [Test]
        public void NonEqualityTest2()
        {
            Maybe<int> m1 = Maybe.Some(2);
            Maybe<int> m2 = Maybe.Some(3);

            Assert.AreNotEqual(m1, m2);
        }

        [Test]
        public void EqualityOperatorTest()
        {
            Maybe<int> m1 = Maybe.Some(2);
            Maybe<int> m2 = Maybe.Some(2);

            Assert.IsTrue(m1 == m2);
        }

        [Test]
        public void EqualityOperatorTestNone()
        {
            Maybe<int> m1 = Maybe.None;
            Maybe<int> m2 = Maybe.None;

            Assert.IsTrue(m1 == m2);
        }

        [Test]
        public void NonEqualityOperatorTest1()
        {
            Maybe<int> m1 = Maybe.Some(2);
            Maybe<int> m2 = Maybe.None;

            Assert.IsTrue(m1 != m2);
        }

        [Test]
        public void NonEqualityOperatorTest2()
        {
            Maybe<int> m1 = Maybe.Some(2);
            Maybe<int> m2 = Maybe.Some(3);

            Assert.IsTrue(m1 != m2);
        }
    }
}