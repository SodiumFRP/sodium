using NUnit.Framework;

namespace Sodium.Tests.Internal
{
    [TestFixture]
    public class NodeTester
    {
        [Test]
        public void TestNode()
        {
            Node<int> a = new Node<int>(0);
            Node<int> b = new Node<int>(0);
            Transaction.Apply(trans =>
            {
                a.Link(trans, (t, v) => { }, b);
                trans.Prioritized(a, t => { });
                return Unit.Value;
            });
            Assert.That(a, Is.LessThan(b));
        }
    }
}