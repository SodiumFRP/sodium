using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class MaybeTests
    {
        [Test]
        public void DefaultValueTest()
        {
            Maybe<int> m = new Maybe<int>();

            int? n = m.Match(v => (int?)v, () => null);

            Assert.IsFalse(n.HasValue);
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

            int n = m.Match(v => v, () => 0);

            Assert.AreEqual(0, n);
        }

        [Test]
        public void EqualityTest()
        {
            Maybe<int> m1 = Maybe.Some(2);
            Maybe<int> m2 = Maybe.Some(2);

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
    }
}