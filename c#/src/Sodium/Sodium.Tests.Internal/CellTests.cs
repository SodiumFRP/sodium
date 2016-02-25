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
            Transaction.Run(trans => trans.Prioritized(Node<Unit>.Null, trans2 => calledBack = true));
            Assert.IsTrue(calledBack);
        }
    }
}