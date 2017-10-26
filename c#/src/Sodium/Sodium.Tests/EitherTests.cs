using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class EitherTests
    {
        [Test]
        public void Either4Test()
        {
            Either<Test1, Test2, Test3, Test4> v1 = Either.First(new Test1());
            Either<Test1, Test2, Test3, Test4> v2 = Either.Second(new Test2());
            Either<Test1, Test2, Test3, Test4> v3 = Either.Third(new Test3());
            Either<Test1, Test2, Test3, Test4> v4 = Either.Fourth(new Test4());

            Assert.AreEqual(1, TestIt(v1));
            Assert.AreEqual(2, TestIt(v2));
            Assert.AreEqual(3, TestIt(v3));
            Assert.AreEqual(4, TestIt(v4));

            Assert.AreEqual(1, TestIt2(v1));
            Assert.AreEqual(2, TestIt2(v2));
            Assert.AreEqual(3, TestIt2(v3));
            Assert.AreEqual(4, TestIt2(v4));
        }

        private static int TestIt(Either<Test1, Test2, Test3, Test4> e) => e.Switch(v1 => 1, v2 => 2, v3 => 3, v4 => 4);

        private static int TestIt2(Either<Test1, Test2, Test3, Test4> e)
        {
            int n = 0;
            e.Switch(v1 => { n = 1; }, v2 => { n = 2; }, v3 => { n = 3; }, v4 => { n = 4; });
            return n;
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
    }
}