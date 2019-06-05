using System.Collections.Generic;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests
{
    [TestFixture]
    public class MaybeExtensionMethodsTests
    {
        [Test]
        public void TestFlatten1()
        {
            Maybe<Maybe<int>> m = Maybe.None;

            Maybe<int> result = m.Flatten();

            Assert.AreEqual(Maybe<int>.None, result);
        }

        [Test]
        public void TestFlatten2()
        {
            Maybe<Maybe<int>> m = Maybe.Some(Maybe<int>.None);

            Maybe<int> result = m.Flatten();

            Assert.AreEqual(Maybe<int>.None, result);
        }

        [Test]
        public void TestFlatten3()
        {
            Maybe<Maybe<int>> m = Maybe.Some(Maybe.Some(5));

            Maybe<int> result = m.Flatten();

            Assert.AreEqual(Maybe.Some(5), result);
        }

        [Test]
        public void TestWhereMaybeNone()
        {
            Maybe<int>[] m = { Maybe<int>.None, Maybe<int>.None, Maybe<int>.None, Maybe<int>.None, Maybe<int>.None };

            IEnumerable<int> result = m.WhereMaybe();

            CollectionAssert.AreEqual(new int[0], result);
        }

        [Test]
        public void TestWhereMaybeSome()
        {
            Maybe<int>[] m = { Maybe<int>.None, Maybe.Some(2), Maybe.Some(5), Maybe<int>.None, Maybe.Some(7) };

            IEnumerable<int> result = m.WhereMaybe();

            CollectionAssert.AreEqual(new[] { 2, 5, 7 }, result);
        }

        [Test]
        public void TestWhereMaybeAll()
        {
            Maybe<int>[] m = { Maybe.Some(3), Maybe.Some(2), Maybe.Some(5), Maybe.Some(4), Maybe.Some(7) };

            IEnumerable<int> result = m.WhereMaybe();

            CollectionAssert.AreEqual(new[] { 3, 2, 5, 4, 7 }, result);
        }

        [Test]
        public void TestAllMaybeOrNoneNone()
        {
            Maybe<int>[] m = { Maybe<int>.None, Maybe<int>.None, Maybe<int>.None, Maybe<int>.None, Maybe<int>.None };

            Maybe<IEnumerable<int>> result = m.AllMaybeOrNone();

            Assert.AreEqual(Maybe<IEnumerable<int>>.None, result);
        }

        [Test]
        public void TestAllMaybeOrNoneSome()
        {
            Maybe<int>[] m = { Maybe<int>.None, Maybe.Some(2), Maybe.Some(5), Maybe<int>.None, Maybe.Some(7) };

            Maybe<IEnumerable<int>> result = m.AllMaybeOrNone();

            Assert.AreEqual(Maybe<IEnumerable<int>>.None, result);
        }

        [Test]
        public void TestAllMaybeOrNoneAll()
        {
            Maybe<int>[] m = { Maybe.Some(3), Maybe.Some(2), Maybe.Some(5), Maybe.Some(4), Maybe.Some(7) };

            Maybe<IEnumerable<int>> result = m.AllMaybeOrNone();

            IEnumerable<int> r = result.Match(v => v, () => null);
            Assert.IsNotNull(r);
            CollectionAssert.AreEqual(new[] { 3, 2, 5, 4, 7 }, r);
        }
    }
}