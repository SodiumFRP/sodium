from typing import List

from sodiumfrp import operational
from sodiumfrp.stream import Cell, CellSink, StreamSink

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

def test_apply() -> None:
    bf = CellSink(lambda b: f"1 {b}")
    ba = CellSink(5)
    out: List[str] = []
    l = Cell.apply(bf, ba).listen(out.append)
    bf.send(lambda b: f"12 {b}")
    ba.send(6)
    l.unlisten()
    assert ["1 5", "12 5", "12 6"] == out

def test_listen() -> None:
    b = CellSink(9)
    out: List[int] = []
    l = b.listen(out.append)
    b.send(2)
    b.send(7)
    l.unlisten()
    assert [9, 2, 7] == out
