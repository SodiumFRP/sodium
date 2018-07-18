using System;
using System.Collections.Generic;
using System.Linq;
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
            Transaction.Apply(
                trans =>
                {
                    trans.Prioritized(Node<Unit>.Null, trans2 => calledBack = true);
                    return Unit.Value;
                },
                false);
            Assert.IsTrue(calledBack);
        }

        [Test]
        public void TestRegen()
        {
            List<int> @out = new List<int>();
            
            Transaction.Apply(
                trans =>
                {
                    void SetNeedsRegeneratingAndPrioritized(Action action)
                    {
                        trans.SetNeedsRegenerating();
                        trans.Prioritized(new Node<Unit>(), _ => action());
                    }
                    
                    SetNeedsRegeneratingAndPrioritized(() => @out.Add(1));
                    SetNeedsRegeneratingAndPrioritized(() => SetNeedsRegeneratingAndPrioritized(() => @out.Add(4)));
                    SetNeedsRegeneratingAndPrioritized(() => @out.Add(2));
                    SetNeedsRegeneratingAndPrioritized(
                        () => SetNeedsRegeneratingAndPrioritized(
                            () => SetNeedsRegeneratingAndPrioritized(() => @out.Add(6))));
                    SetNeedsRegeneratingAndPrioritized(() => SetNeedsRegeneratingAndPrioritized(() => @out.Add(5)));
                    trans.Prioritized(new Node<Unit>(), trans2 => @out.Add(3));

                    return Unit.Value;
                },
                false);
            CollectionAssert.AreEqual(new[] { 1, 2, 3, 4, 5, 6 }, @out);
        }
    }
}