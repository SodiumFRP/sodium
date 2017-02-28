using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class DiscreteCellTests
    {
        [Test]
        public void TestLoop()
        {
            Tuple<DiscreteCell<int>, DiscreteCellStreamSink<int>> result = Transaction.Run(() =>
             {
                 DiscreteCellLoop<int> l = DiscreteCell.CreateLoop<int>();
                 DiscreteCell<int> cLocal = l.Map(v => v * 5);
                 DiscreteCellStreamSink<int> sLocal = new DiscreteCellStreamSink<int>();
                 l.Loop(sLocal.ToDiscreteCell(3));
                 return Tuple.Create(cLocal, sLocal);
             });

            DiscreteCell<int> c = result.Item1;
            DiscreteCellStreamSink<int> s = result.Item2;

            List<int> output1 = new List<int>();
            List<int> output2 = new List<int>();
            c.Cell.Listen(output1.Add);
            c.Updates.Listen(output2.Add);

            s.Send(5);
            s.Send(7);

            CollectionAssert.AreEqual(new[] { 15, 25, 35 }, output1);
            CollectionAssert.AreEqual(new[] { 25, 35 }, output2);
        }
    }
}