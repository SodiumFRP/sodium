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
