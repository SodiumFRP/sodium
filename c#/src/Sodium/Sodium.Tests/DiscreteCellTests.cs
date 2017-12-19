using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class DiscreteCellTests
    {
        [Test]
        public void TestLoop()
        {
            (DiscreteCell<int> c, DiscreteCellStreamSink<int> s) = Transaction.Run(() =>
             {
                 DiscreteCellLoop<int> loop = DiscreteCell.CreateLoop<int>();
                 DiscreteCell<int> cLocal = loop.Map(v => v * 5);
                 DiscreteCellStreamSink<int> sLocal = new DiscreteCellStreamSink<int>();
                 loop.Loop(sLocal.Hold(3));
                 return (cLocal, sLocal);
             });

            List<int> output1 = new List<int>();
            List<int> output2 = new List<int>();
            IListener l = c.Listen(output1.Add);
            IListener l2 = c.Updates.Listen(output2.Add);

            s.Send(5);
            s.Send(7);

            l2.Unlisten();
            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 15, 25, 35 }, output1);
            CollectionAssert.AreEqual(new[] { 25, 35 }, output2);
        }

        [Test]
        public void TestLiftSimultaneousUpdates()
        {
            List<int> @out = new List<int>();
            DiscreteCellSink<int> cellSink = DiscreteCell.CreateSink(1);
            DiscreteCell<int> cell = cellSink.Map(v => 2 * v);
            IListener l = cellSink.Lift(cell, (x, y) => x + y).Updates.Listen(@out.Add);

            cellSink.Send(2);
            cellSink.Send(7);

            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 6, 21 }, @out);
        }

        [Test]
        public void TestLiftInSwitchC()
        {
            IReadOnlyList<Test> list1 = new[] { new Test(0), new Test(1), new Test(2), new Test(3), new Test(4) };
            IReadOnlyList<Test> list2 = new[] { new Test(5), new Test(6), new Test(7), new Test(8), new Test(9) };

            DiscreteCellSink<IReadOnlyList<Test>> v = DiscreteCell.CreateSink(list1);

            DiscreteCell<IReadOnlyList<int>> c = v.Map(oo => oo.Select(o => o.Value).Lift()).SwitchC();

            List<IReadOnlyList<int>> streamOutput = new List<IReadOnlyList<int>>();
            IListener l = c.Updates.Listen(streamOutput.Add);

            List<IReadOnlyList<int>> cellOutput = new List<IReadOnlyList<int>>();
            IListener l2 = c.Listen(cellOutput.Add);

            list1[2].Value.Send(12);
            list2[1].Value.Send(16);
            list1[4].Value.Send(14);
            Transaction.RunVoid(() =>
            {
                list2[2].Value.Send(17);
                list1[0].Value.Send(10);
                v.Send(list2);
            });
            list1[3].Value.Send(13);
            list2[3].Value.Send(18);

            l2.Unlisten();
            l.Unlisten();

            Assert.AreEqual(4, streamOutput.Count);
            Assert.AreEqual(5, cellOutput.Count);

            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4 }, cellOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, streamOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, cellOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, streamOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, cellOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, streamOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, cellOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, streamOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, cellOutput[4]);
        }

        [Test]
        public void TestMapWithSwitchC()
        {
            IReadOnlyList<Test> list1 = new[] { new Test(0), new Test(1), new Test(2), new Test(3), new Test(4) };
            IReadOnlyList<Test> list2 = new[] { new Test(5), new Test(6), new Test(7), new Test(8), new Test(9) };

            DiscreteCellSink<IReadOnlyList<Test>> v = DiscreteCell.CreateSink(list1);

            DiscreteCell<IReadOnlyList<int>> c = v.Map(oo => oo.Select(o => o.Value).Lift()).Map(o => o).SwitchC();

            List<IReadOnlyList<int>> streamOutput = new List<IReadOnlyList<int>>();
            IListener l = c.Updates.Listen(streamOutput.Add);

            List<IReadOnlyList<int>> cellOutput = new List<IReadOnlyList<int>>();
            IListener l2 = c.Listen(cellOutput.Add);

            list1[2].Value.Send(12);
            list2[1].Value.Send(16);
            list1[4].Value.Send(14);
            Transaction.RunVoid(() =>
            {
                list2[2].Value.Send(17);
                list1[0].Value.Send(10);
                v.Send(list2);
            });
            list1[3].Value.Send(13);
            list2[3].Value.Send(18);

            l2.Unlisten();
            l.Unlisten();

            Assert.AreEqual(4, streamOutput.Count);
            Assert.AreEqual(5, cellOutput.Count);

            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4 }, cellOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, streamOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, cellOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, streamOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, cellOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, streamOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, cellOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, streamOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, cellOutput[4]);
        }

        public class Test
        {
            public Test(int initialValue)
            {
                this.Value = DiscreteCell.CreateSink(initialValue);
            }

            public DiscreteCellSink<int> Value { get; }
        }
    }
}