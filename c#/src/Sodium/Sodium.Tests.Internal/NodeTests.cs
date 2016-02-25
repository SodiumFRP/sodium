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
            Node<int> b = new Node<int>(1);
            a.Link((t, v) => { }, b);
            Assert.That(a, Is.LessThan(b));
        }
    }
}