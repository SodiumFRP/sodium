from dataclasses import dataclass
from typing import List, Tuple

from sodiumfrp import operational
from sodiumfrp.stream import Cell, CellSink, StreamSink
from sodiumfrp.transaction import Transaction

def test_hold() -> None:
    e: StreamSink[int] = StreamSink()
    b = e.hold(0)
    out: List[int] = []
    l = operational.updates(b).listen(out.append)
    e.send(2)
    e.send(9)
    l.unlisten()
    assert [2, 9] == out

def test_snapshot() -> None:
    b: CellSink[int] = CellSink(0)
    trigger: StreamSink[int] = StreamSink()
    out: List[str] = []
    l = trigger \
        .snapshot(b, lambda x, y: f"{x} {y}") \
        .listen(out.append)
    trigger.send(100)
    b.send(2)
    trigger.send(200)
    b.send(9)
    b.send(1)
    trigger.send(300)
    l.unlisten()
    assert ["100 0", "200 2", "300 1"] == out

def test_hold_is_delayed() -> None:
    e: StreamSink[int] = StreamSink()
    h = e.hold(0)
    pair = e.snapshot(h, lambda a, b: f"{a} {b}")
    out: List[str] = []
    l = pair.listen(out.append)
    e.send(2)
    e.send(3)
    l.unlisten()
    assert ["2 0", "3 2"] == out

def test_sample() -> None:
    b: CellSink = CellSink(0)
    out: List[int] = []
    out.append(b.sample())
    b.send(3)
    out.append(b.sample())
    b.send(42)
    out.append(b.sample())
    assert [0, 3, 42] == out

def test_map() -> None:
    b = CellSink(6)
    out: List[str] = []
    l = b.map(str).listen(out.append)
    b.send(8)
    l.unlisten()
    assert ["6", "8"] == out

def test_map_late_listen() -> None:
    b = CellSink(6)
    out: List[str] = []
    bm = b.map(str)
    b.send(2)
    l = bm.listen(out.append)
    b.send(8)
    l.unlisten()
    assert ["2", "8"] == out

def test_apply() -> None:
    bf = CellSink(lambda b: f"1 {b}")
    ba = CellSink(5)
    out: List[str] = []
    l = Cell.apply(bf, ba).listen(out.append)
    bf.send(lambda b: f"12 {b}")
    ba.send(6)
    l.unlisten()
    assert ["1 5", "12 5", "12 6"] == out

def test_lift() -> None:
    a = CellSink(1)
    b = CellSink(5)
    out: List[str] = []
    l = a.lift(b, lambda x, y: f"{x} {y}").listen(out.append)
    a.send(12)
    b.send(6)
    l.unlisten()
    assert ["1 5", "12 5", "12 6"] == out

def test_lift_glitch() -> None:
    a = CellSink(1)
    a3 = a.map(lambda x: x * 3)
    a5 = a.map(lambda x: x * 5)
    b = a3.lift(a5, lambda x, y: f"{x} {y}")
    out: List[str] = []
    l = b.listen(out.append)
    a.send(2)
    l.unlisten()
    assert ["3 5", "6 10"] == out

def test_lift_from_simultaneous() -> None:
    def transaction() -> Tuple[Cell,Cell]:
        b1 = CellSink(3)
        b2 = CellSink(5)
        b2.send(7)
        return (b1, b2)
    b1, b2 = Transaction.run(transaction)
    out: List[int] = []
    l = b1.lift(b2, lambda x, y: x + y).listen(out.append)
    l.unlisten()
    assert [10] == out

def test_listen() -> None:
    b = CellSink(9)
    out: List[int] = []
    l = b.listen(out.append)
    b.send(2)
    b.send(7)
    l.unlisten()
    assert [9, 2, 7] == out

def test_constant() -> None:
    b = Cell.constant(12)
    out: List[int] = []
    l = b.listen(out.append)
    l.unlisten()
    assert [12] == out

def test_switch_cell() -> None:

    @dataclass
    class SB:
        a: str
        b: str
        sw: Cell[str]

    esb: StreamSink[SB] = StreamSink()
    # Split each field out of SB so we can update multiple behaviours in a
    # single transaction.
    not_none = lambda x: x is not None
    ba = esb.map(lambda s: s.a).filter(not_none).hold("A")
    bb = esb.map(lambda s: s.b).filter(not_none).hold("a")
    bsw = esb.map(lambda s: s.sw).filter(not_none).hold(ba)
    bo = Cell.switch_cell(bsw)
    out: List[str] = []
    l = bo.listen(out.append)
    esb.send(SB("B", "b", None))
    esb.send(SB("C", "c", bb))
    esb.send(SB("D", "d", None))
    esb.send(SB("E", "e", ba))
    esb.send(SB("F", "f", None))
    esb.send(SB(None, None, bb))
    esb.send(SB(None, None, ba))
    esb.send(SB("G", "g", bb))
    esb.send(SB("H", "h", ba))
    esb.send(SB("I", "i", ba))
    l.unlisten()
    assert ["A", "B", "c", "d", "E", "F", "f", "F", "g", "H", "I"] == out
