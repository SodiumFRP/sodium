using NUnit.Framework;

namespace Sodium.Tests.Internal
{
    [TestFixture]
    public class CellTests
    {
        [Test]
        public void TestTransaction()
        {
            bool calledBack = false;
            Transaction.Apply(trans =>
            {
                trans.Prioritized(Node<Unit>.Null, trans2 => calledBack = true);
                return Unit.Value;
            }, false);
            Assert.IsTrue(calledBack);
        }
    }
}