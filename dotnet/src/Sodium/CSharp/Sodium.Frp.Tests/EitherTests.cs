using System;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests
{
    [TestFixture]
    public class EitherTests
    {
        [Test]
        public void DefaultValue2Test()
        {
            Either<Test1, Test2> e = default(Either<Test1, Test2>);

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt2(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultValue3Test()
        {
            Either<Test1, Test2, Test3> e = default(Either<Test1, Test2, Test3>);

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt3(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultValue4Test()
        {
            Either<Test1, Test2, Test3, Test4> e = default(Either<Test1, Test2, Test3, Test4>);

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt4(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultValue5Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5> e = default(Either<Test1, Test2, Test3, Test4, Test5>);

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt5(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultValue6Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5, Test6> e = default(Either<Test1, Test2, Test3, Test4, Test5, Test6>);

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt6(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultValue7Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> e = default(Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7>);

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt7(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultValue8Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> e = default(Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8>);

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt8(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultConstructor2Test()
        {
            Either<Test1, Test2> e = new Either<Test1, Test2>();

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt2(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultConstructor3Test()
        {
            Either<Test1, Test2, Test3> e = new Either<Test1, Test2, Test3>();

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt3(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultConstructor4Test()
        {
            Either<Test1, Test2, Test3, Test4> e = new Either<Test1, Test2, Test3, Test4>();

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt4(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultConstructor5Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5> e = new Either<Test1, Test2, Test3, Test4, Test5>();

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt5(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultConstructor6Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5, Test6> e = new Either<Test1, Test2, Test3, Test4, Test5, Test6>();

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt6(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultConstructor7Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> e = new Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7>();

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt7(e));
            Assert.IsNull(o);
        }

        [Test]
        public void DefaultConstructor8Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> e = new Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8>();

            object o = e.Upcast<IEither>().GetValueAsObject();

            Assert.AreEqual(1, TestIt8(e));
            Assert.IsNull(o);
        }

        [Test]
        public void Either2Test()
        {
            Either<Test1, Test2> v1 = Either.First(new Test1());
            Either<Test1, Test2> v2 = Either.Second(new Test2());

            Assert.AreEqual(1, TestIt2(v1));
            Assert.AreEqual(2, TestIt2(v2));

            Assert.AreEqual(1, TestIt2Action(v1));
            Assert.AreEqual(2, TestIt2Action(v2));
        }

        [Test]
        public void Either3Test()
        {
            Either<Test1, Test2, Test3> v1 = Either.First(new Test1());
            Either<Test1, Test2, Test3> v2 = Either.Second(new Test2());
            Either<Test1, Test2, Test3> v3 = Either.Third(new Test3());

            Assert.AreEqual(1, TestIt3(v1));
            Assert.AreEqual(2, TestIt3(v2));
            Assert.AreEqual(3, TestIt3(v3));

            Assert.AreEqual(1, TestIt3Action(v1));
            Assert.AreEqual(2, TestIt3Action(v2));
            Assert.AreEqual(3, TestIt3Action(v3));
        }

        [Test]
        public void Either4Test()
        {
            Either<Test1, Test2, Test3, Test4> v1 = Either.First(new Test1());
            Either<Test1, Test2, Test3, Test4> v2 = Either.Second(new Test2());
            Either<Test1, Test2, Test3, Test4> v3 = Either.Third(new Test3());
            Either<Test1, Test2, Test3, Test4> v4 = Either.Fourth(new Test4());

            Assert.AreEqual(1, TestIt4(v1));
            Assert.AreEqual(2, TestIt4(v2));
            Assert.AreEqual(3, TestIt4(v3));
            Assert.AreEqual(4, TestIt4(v4));

            Assert.AreEqual(1, TestIt4Action(v1));
            Assert.AreEqual(2, TestIt4Action(v2));
            Assert.AreEqual(3, TestIt4Action(v3));
            Assert.AreEqual(4, TestIt4Action(v4));
        }

        [Test]
        public void Either5Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5> v1 = Either.First(new Test1());
            Either<Test1, Test2, Test3, Test4, Test5> v2 = Either.Second(new Test2());
            Either<Test1, Test2, Test3, Test4, Test5> v3 = Either.Third(new Test3());
            Either<Test1, Test2, Test3, Test4, Test5> v4 = Either.Fourth(new Test4());
            Either<Test1, Test2, Test3, Test4, Test5> v5 = Either.Fifth(new Test5());

            Assert.AreEqual(1, TestIt5(v1));
            Assert.AreEqual(2, TestIt5(v2));
            Assert.AreEqual(3, TestIt5(v3));
            Assert.AreEqual(4, TestIt5(v4));
            Assert.AreEqual(5, TestIt5(v5));

            Assert.AreEqual(1, TestIt5Action(v1));
            Assert.AreEqual(2, TestIt5Action(v2));
            Assert.AreEqual(3, TestIt5Action(v3));
            Assert.AreEqual(4, TestIt5Action(v4));
            Assert.AreEqual(5, TestIt5Action(v5));
        }

        [Test]
        public void Either6Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5, Test6> v1 = Either.First(new Test1());
            Either<Test1, Test2, Test3, Test4, Test5, Test6> v2 = Either.Second(new Test2());
            Either<Test1, Test2, Test3, Test4, Test5, Test6> v3 = Either.Third(new Test3());
            Either<Test1, Test2, Test3, Test4, Test5, Test6> v4 = Either.Fourth(new Test4());
            Either<Test1, Test2, Test3, Test4, Test5, Test6> v5 = Either.Fifth(new Test5());
            Either<Test1, Test2, Test3, Test4, Test5, Test6> v6 = Either.Sixth(new Test6());

            Assert.AreEqual(1, TestIt6(v1));
            Assert.AreEqual(2, TestIt6(v2));
            Assert.AreEqual(3, TestIt6(v3));
            Assert.AreEqual(4, TestIt6(v4));
            Assert.AreEqual(5, TestIt6(v5));
            Assert.AreEqual(6, TestIt6(v6));

            Assert.AreEqual(1, TestIt6Action(v1));
            Assert.AreEqual(2, TestIt6Action(v2));
            Assert.AreEqual(3, TestIt6Action(v3));
            Assert.AreEqual(4, TestIt6Action(v4));
            Assert.AreEqual(5, TestIt6Action(v5));
            Assert.AreEqual(6, TestIt6Action(v6));
        }

        [Test]
        public void Either7Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> v1 = Either.First(new Test1());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> v2 = Either.Second(new Test2());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> v3 = Either.Third(new Test3());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> v4 = Either.Fourth(new Test4());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> v5 = Either.Fifth(new Test5());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> v6 = Either.Sixth(new Test6());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> v7 = Either.Seventh(new Test7());

            Assert.AreEqual(1, TestIt7(v1));
            Assert.AreEqual(2, TestIt7(v2));
            Assert.AreEqual(3, TestIt7(v3));
            Assert.AreEqual(4, TestIt7(v4));
            Assert.AreEqual(5, TestIt7(v5));
            Assert.AreEqual(6, TestIt7(v6));
            Assert.AreEqual(7, TestIt7(v7));

            Assert.AreEqual(1, TestIt7Action(v1));
            Assert.AreEqual(2, TestIt7Action(v2));
            Assert.AreEqual(3, TestIt7Action(v3));
            Assert.AreEqual(4, TestIt7Action(v4));
            Assert.AreEqual(5, TestIt7Action(v5));
            Assert.AreEqual(6, TestIt7Action(v6));
            Assert.AreEqual(7, TestIt7Action(v7));
        }

        [Test]
        public void Either8Test()
        {
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> v1 = Either.First(new Test1());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> v2 = Either.Second(new Test2());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> v3 = Either.Third(new Test3());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> v4 = Either.Fourth(new Test4());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> v5 = Either.Fifth(new Test5());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> v6 = Either.Sixth(new Test6());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> v7 = Either.Seventh(new Test7());
            Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> v8 = Either.Eighth(new Test8());

            Assert.AreEqual(1, TestIt8(v1));
            Assert.AreEqual(2, TestIt8(v2));
            Assert.AreEqual(3, TestIt8(v3));
            Assert.AreEqual(4, TestIt8(v4));
            Assert.AreEqual(5, TestIt8(v5));
            Assert.AreEqual(6, TestIt8(v6));
            Assert.AreEqual(7, TestIt8(v7));
            Assert.AreEqual(8, TestIt8(v8));

            Assert.AreEqual(1, TestIt8Action(v1));
            Assert.AreEqual(2, TestIt8Action(v2));
            Assert.AreEqual(3, TestIt8Action(v3));
            Assert.AreEqual(4, TestIt8Action(v4));
            Assert.AreEqual(5, TestIt8Action(v5));
            Assert.AreEqual(6, TestIt8Action(v6));
            Assert.AreEqual(7, TestIt8Action(v7));
            Assert.AreEqual(8, TestIt8Action(v8));
        }

        [Test]
        public void EqualityTest()
        {
            Either<int, double, DateTime, string, char, float, long, byte> e1 = Either.Seventh(2L);
            Either<int, double, DateTime, string, char, float, long, byte> e2 = Either.Seventh(2L);

            Assert.AreEqual(e1, e2);
        }

        [Test]
        public void NonEqualityTest1()
        {
            Either<int, double, DateTime, string, char, float, long, byte> e1 = Either.Seventh(2L);
            Either<int, double, DateTime, string, char, float, long, byte> e2 = Either.First(2);

            Assert.AreNotEqual(e1, e2);
        }

        [Test]
        public void NonEqualityTest2()
        {
            Either<int, double, DateTime, string, char, float, long, byte> e1 = Either.Seventh(2L);
            Either<int, double, DateTime, string, char, float, long, byte> e2 = Either.Seventh(3L);

            Assert.AreNotEqual(e1, e2);
        }

        [Test]
        public void EqualityOperatorTest()
        {
            Either<int, double, DateTime, string, char, float, long, byte> e1 = Either.Seventh(2L);
            Either<int, double, DateTime, string, char, float, long, byte> e2 = Either.Seventh(2L);

            Assert.IsTrue(e1 == e2);
        }

        [Test]
        public void NonEqualityOperatorTest1()
        {
            Either<int, double, DateTime, string, char, float, long, byte> e1 = Either.Seventh(2L);
            Either<int, double, DateTime, string, char, float, long, byte> e2 = Either.First(2);

            Assert.IsTrue(e1 != e2);
        }

        [Test]
        public void NonEqualityOperatorTest2()
        {
            Either<int, double, DateTime, string, char, float, long, byte> e1 = Either.Seventh(2L);
            Either<int, double, DateTime, string, char, float, long, byte> e2 = Either.Seventh(3L);

            Assert.IsTrue(e1 != e2);
        }

        private class Test1
        {
        }

        private class Test2
        {
        }

        private class Test3
        {
        }

        private class Test4
        {
        }

        private class Test5
        {
        }

        private class Test6
        {
        }

        private class Test7
        {
        }

        private class Test8
        {
        }

        private static int TestIt2(Either<Test1, Test2> e) => e.Match(v1 => 1, v2 => 2);

        private static int TestIt2Action(Either<Test1, Test2> e)
        {
            int n = 0;
            e.MatchVoid(v1 => { n = 1; }, v2 => { n = 2; });
            return n;
        }

        private static int TestIt3(Either<Test1, Test2, Test3> e) => e.Match(v1 => 1, v2 => 2, v3 => 3);

        private static int TestIt3Action(Either<Test1, Test2, Test3> e)
        {
            int n = 0;
            e.MatchVoid(v1 => { n = 1; }, v2 => { n = 2; }, v3 => { n = 3; });
            return n;
        }

        private static int TestIt4(Either<Test1, Test2, Test3, Test4> e) => e.Match(v1 => 1, v2 => 2, v3 => 3, v4 => 4);

        private static int TestIt4Action(Either<Test1, Test2, Test3, Test4> e)
        {
            int n = 0;
            e.MatchVoid(v1 => { n = 1; }, v2 => { n = 2; }, v3 => { n = 3; }, v4 => { n = 4; });
            return n;
        }

        private static int TestIt5(Either<Test1, Test2, Test3, Test4, Test5> e) => e.Match(v1 => 1, v2 => 2, v3 => 3, v4 => 4, v5 => 5);

        private static int TestIt5Action(Either<Test1, Test2, Test3, Test4, Test5> e)
        {
            int n = 0;
            e.MatchVoid(v1 => { n = 1; }, v2 => { n = 2; }, v3 => { n = 3; }, v4 => { n = 4; }, v5 => { n = 5; });
            return n;
        }

        private static int TestIt6(Either<Test1, Test2, Test3, Test4, Test5, Test6> e) => e.Match(v1 => 1, v2 => 2, v3 => 3, v4 => 4, v5 => 5, v6 => 6);

        private static int TestIt6Action(Either<Test1, Test2, Test3, Test4, Test5, Test6> e)
        {
            int n = 0;
            e.MatchVoid(v1 => { n = 1; }, v2 => { n = 2; }, v3 => { n = 3; }, v4 => { n = 4; }, v5 => { n = 5; }, v6 => { n = 6; });
            return n;
        }

        private static int TestIt7(Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> e) => e.Match(v1 => 1, v2 => 2, v3 => 3, v4 => 4, v5 => 5, v6 => 6, v7 => 7);

        private static int TestIt7Action(Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7> e)
        {
            int n = 0;
            e.MatchVoid(v1 => { n = 1; }, v2 => { n = 2; }, v3 => { n = 3; }, v4 => { n = 4; }, v5 => { n = 5; }, v6 => { n = 6; }, v7 => { n = 7; });
            return n;
        }

        private static int TestIt8(Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> e) => e.Match(v1 => 1, v2 => 2, v3 => 3, v4 => 4, v5 => 5, v6 => 6, v7 => 7, v8 => 8);

        private static int TestIt8Action(Either<Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8> e)
        {
            int n = 0;
            e.MatchVoid(v1 => { n = 1; }, v2 => { n = 2; }, v3 => { n = 3; }, v4 => { n = 4; }, v5 => { n = 5; }, v6 => { n = 6; }, v7 => { n = 7; }, v8 => { n = 8; });
            return n;
        }
    }
}