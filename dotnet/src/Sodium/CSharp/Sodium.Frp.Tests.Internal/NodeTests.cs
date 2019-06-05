using NUnit.Framework;

namespace Sodium.Frp.Tests.Internal
{
    [TestFixture]
    public class NodeTests
    {
        [Test]
        public void TestNode()
        {
            Node<int> a = new Node<int>();
            Node<int> b = new Node<int>();
            TransactionInternal.Apply(
                (trans, _) =>
                {
                    a.Link(trans, (t, v) => { }, b);
                    trans.Prioritized(a, t => { });
                    return UnitInternal.Value;
                },
                false);
            Assert.That(a.Rank, Is.LessThan(b.Rank));
        }
    }
}