package sodium

import org.junit.Assert.assertEquals
import org.junit.{After, Test}

import scala.collection.mutable.ListBuffer

class CellTester {

  @After def tearDown(): Unit = {
    System.gc()
    Thread.sleep(100)
  }

  @Test
  def testLoop(): Unit = {
    val (c, s) = Transaction(_ => {
      val loop = new CellLoop[Int]()
      val clocal = loop.map(v => v * 5)
      val slocal = new StreamSink[Int]()
      loop.loop((slocal.hold(3)))
      (clocal, slocal)
    })
    val out1 = new ListBuffer[Int]()
    val out2 = new ListBuffer[Int]()
    val l1 = c.listen(out1.+=)
    val l2 = c.updates().listen(out2.+=)
    s.send(5)
    s.send(7)
    l2.unlisten()
    l1.unlisten()
    assertEquals(List(15, 25, 35), out1)
    assertEquals(List(25, 35), out2)
  }

  @Test
  def testLiftSimultaneousUpdates(): Unit = {
    val out = new ListBuffer[Int]()
    val cellSink = new CellSink(1)
    val cell = cellSink.map(v => 2 * v)
    val l = cellSink.lift(cell, (x: Int, y: Int) => x + y).updates().listen(out.+=)
    cellSink.send(2)
    cellSink.send(7)
    l.unlisten()
    assertEquals(List(6, 21), out)
  }

  @Test
  def testLiftCellsInSwitchC(): Unit = {
    val out = new ListBuffer[Int]()
    val s = new CellSink(0)
    val c = new Cell[Cell[Int]](new Cell(1))
    val r = c.map(c2 => c2.lift[Int, Int](s, (v1, v2) => v1 + v2))
    Cell.switchC(r).listen(out.+=)
    s.send(2)
    s.send(4)
    assertEquals(List(1, 3, 5), out)
  }

//TODO https://github.com/SodiumFRP/sodium/issues/137
  /*
  @Test
  def testLazyCellCreation(): Unit = {
    val out = new ListBuffer[Int]()
    val s: StreamSink[Int] = new StreamSink()
    val c: Cell[Cell[Int]] = new Cell(1).map(_ => s.hold(0))
    s.send(1)
    Cell.switchC(c).listen(out.+=)
    s.send(3)
    s.send(5)
    assertEquals(List(1, 3, 5), out)
  }
 */

}
