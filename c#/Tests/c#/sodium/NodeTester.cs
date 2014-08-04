using NUnit.Framework;

using Sodium;

namespace Tests.sodium
{
  [TestFixture]
  public class NodeTester
  {
    [Test]
    public void TestNode()
    {
      var a = new Node(0);
      var b = new Node(1);
      a.LinkTo(b);
      Assert.That(a, Is.LessThan(b));
    }
  }
}