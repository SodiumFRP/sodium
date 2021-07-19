from dataclasses import dataclass
from typing import Any, List, Optional, Tuple

from sodiumfrp import operational
from sodiumfrp.listener import Listener
from sodiumfrp.stream import Cell, CellLoop, CellSink, Stream, StreamSink
from sodiumfrp.transaction import Transaction
from sodiumfrp.unit import Unit, UNIT

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

def test_switch_stream() -> None:

    @dataclass
    class SE:
        a: str
        b: str
        sw: Stream[str]

    ese: StreamSink[SE] = StreamSink()
    ea = ese.map(lambda s: s.a)
    eb = ese.map(lambda s: s.b)
    bsw = ese.map(lambda s: s.sw).filter(not_none).hold(ea)
    out: List[str] = []
    eo = Cell.switch_stream(bsw)
    l = eo.listen(out.append)
    ese.send(SE('A', 'a', None))
    ese.send(SE('B', 'b', None))
    ese.send(SE('C', 'c', eb))
    ese.send(SE('D', 'd', None))
    ese.send(SE('E', 'e', ea))
    ese.send(SE('F', 'f', None))
    ese.send(SE('G', 'g', eb))
    ese.send(SE('H', 'h', ea))
    ese.send(SE('I', 'i', ea))
    l.unlisten()
    assert ['A', 'B', 'C', 'd', 'e', 'F', 'G', 'h', 'I'] == out

def test_switch_stream_simultaneous() -> None:

    class SS2:
        def __init__(self) -> None:
            self.s: StreamSink[int] = StreamSink()

    ss1 = SS2()
    css = CellSink(ss1)
    so = Cell.switch_stream(css.map(lambda b: b.s))
    out: List[int] = []
    l = so.listen(out.append)
    ss3 = SS2()
    ss4 = SS2()
    ss2 = SS2()
    ss1.s.send(0)
    ss1.s.send(1)
    ss1.s.send(2)
    css.send(ss2)
    ss1.s.send(7)
    ss2.s.send(3)
    ss2.s.send(4)
    ss3.s.send(2)
    css.send(ss3)
    ss3.s.send(5)
    ss3.s.send(6)
    ss3.s.send(7)
    def transaction() -> None:
        ss3.s.send(8)
        css.send(ss4)
        ss4.s.send(2)
    Transaction.run(transaction)
    ss4.s.send(9)
    l.unlisten()
    assert [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] == out

def test_loop_cell() -> None:
    sa: StreamSink[int] = StreamSink()
    def transaction() -> Cell[int]:
        sum_: CellLoop[int] = CellLoop()
        sum_out_ = sa.snapshot(sum_, lambda x, y: x + y).hold(0)
        sum_.loop(sum_out_)
        return sum_out_
    sum_out = Transaction.run(transaction)
    out: List[int] = []
    l = sum_out.listen(out.append)
    sa.send(2)
    sa.send(3)
    sa.send(1)
    l.unlisten()
    assert [0, 2, 5, 6] == out
    assert 6 == sum_out.sample()

def test_loop_value_snapshot() -> None:
    out: List[str] = []
    def transaction() -> Listener:
        a = Cell.constant("lettuce")
        b: CellLoop[str] = CellLoop()
        eSnap = operational.value(a).snapshot(b, lambda x, y: f"{x} {y}")
        b.loop(Cell.constant("cheese"))
        return eSnap.listen(out.append)
    l = Transaction.run(transaction)
    l.unlisten()
    assert ["lettuce cheese"] == out

def test_loop_value_hold() -> None:
    out: List[str] = []
    def transaction() -> Cell[str]:
        a: CellLoop[str] = CellLoop()
        value_ = operational.value(a).hold("onion")
        a.loop(Cell.constant("cheese"))
        return value_
    value = Transaction.run(transaction)
    eTick: StreamSink[Unit] = StreamSink()
    l = eTick.snapshot(value).listen(out.append)
    eTick.send(UNIT)
    l.unlisten()
    assert ["cheese"] == out

def test_lift_loop() -> None:
    out: List[str] = []
    b = CellSink("kettle")
    def transaction() -> Cell[str]:
        a: CellLoop[str] = CellLoop()
        c_ = a.lift(b, lambda x, y: f"{x} {y}")
        a.loop(Cell.constant("tea"))
        return c_
    c = Transaction.run(transaction)
    l = c.listen(out.append)
    b.send("caddy")
    l.unlisten()
    assert ["tea kettle", "tea caddy"] == out

def not_none(x: Optional[Any]) -> bool:
    return x is not None
