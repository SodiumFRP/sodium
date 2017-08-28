using NUnit.Framework;

namespace Sodium.Tests.Internal
{
    [TestFixture]
    public class NodeTests
    {
        [Test]
        public void TestNode()
        {
            Node<int> a = new Node<int>();
            Node<int> b = new Node<int>();
            Transaction.Apply(trans =>
            {
                a.Link(trans, (t, v) => { }, b);
                trans.Prioritized(a, t => { });
                return Unit.Value;
            }, false);
            Assert.That(a.Rank, Is.LessThan(b.Rank));
        }
    }
}