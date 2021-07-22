from typing import List

from sodiumfrp import operational
from sodiumfrp import StreamSink, Transaction

def test_base_send1() -> None:
    s: StreamSink[str] = Transaction.run(StreamSink)
    out: List[str] = []
    l = Transaction.run(lambda: s.listen(out.append))
    Transaction.run(lambda: s.send("a"))
    Transaction.run(lambda: s.send("b"))
    l.unlisten()
    assert ["a", "b"] == out

def test_operational_split() -> None:
    a: StreamSink[List[str]] = Transaction.run(StreamSink)
    b = Transaction.run(lambda: operational.split(a))
    b_0: List[str] = []
    b_0_l = Transaction.run(lambda: b.listen(b_0.append))
    Transaction.run(lambda: a.send(["a", "b"]))
    b_0_l.unlisten()
    assert ["a", "b"] == b_0

def test_operational_defer1() -> None:
    a: StreamSink[str] = Transaction.run(StreamSink)
    b = Transaction.run(lambda: operational.defer(a))
    b_0: List[str] = []
    b_0_l = Transaction.run(lambda: b.listen(b_0.append))
    Transaction.run(lambda: a.send("a"))
    b_0_l.unlisten()
    assert ["a"] == b_0
    b_1: List[str] = []
    b_1_l = Transaction.run(lambda: b.listen(b_1.append))
    Transaction.run(lambda: a.send("b"))
    b_1_l.unlisten()
    assert ["b"] == b_1

def test_operational_defer2() -> None:
    a: StreamSink[str] = Transaction.run(StreamSink)
    b: StreamSink[str] = Transaction.run(StreamSink)
    c = Transaction.run(lambda: operational.defer(a).or_else(b))
    c_0: List[str] = []
    c_0_l = Transaction.run(lambda: c.listen(c_0.append))
    Transaction.run(lambda: a.send("a"))
    c_0_l.unlisten()
    assert ["a"] == c_0
    c_1: List[str] = []
    c_1_l = Transaction.run(lambda: c.listen(c_1.append))
    def transaction() -> None:
        a.send("b")
        b.send("B")
    Transaction.run(transaction)
    c_1_l.unlisten()
    assert ["B", "b"] == c_1

def test_stream_or_else1() -> None:
    a: StreamSink[int] = Transaction.run(StreamSink)
    b: StreamSink[int] = Transaction.run(StreamSink)
    c = Transaction.run(lambda: a.or_else(b))
    c_0: List[int] = []
    c_0_l = Transaction.run(lambda: c.listen(c_0.append))
    Transaction.run(lambda: a.send(0))
    c_0_l.unlisten()
    assert [0] == c_0
    c_1: List[int] = []
    c_1_l = Transaction.run(lambda: c.listen(c_1.append))
    Transaction.run(lambda: b.send(10))
    c_1_l.unlisten()
    assert [10] == c_1
    c_2: List[int] = []
    c_2_l = Transaction.run(lambda: c.listen(c_2.append))
    def transaction() -> None:
        a.send(2)
        b.send(20)
    Transaction.run(transaction)
    c_2_l.unlisten()
    assert [2] == c_2
    c_3: List[int] = []
    c_3_l = Transaction.run(lambda: c.listen(c_3.append))
    Transaction.run(lambda: b.send(30))
    c_3_l.unlisten()
    assert [30] == c_3

def test_operational_defer_simultaneous() -> None:
    a: StreamSink[str] = Transaction.run(StreamSink)
    b: StreamSink[str] = Transaction.run(StreamSink)
    c = Transaction.run(
        lambda: operational.defer(a).or_else(operational.defer(b)))
    c_0: List[str] = []
    c_0_l = Transaction.run(lambda: c.listen(c_0.append))
    Transaction.run(lambda: b.send("A"))
    c_0_l.unlisten()
    assert ["A"] == c_0
    c_1: List[str] = []
    c_1_l = Transaction.run(lambda: c.listen(c_1.append))
    def transaction() -> None:
        a.send("b")
        b.send("B")
    Transaction.run(transaction)
    c_1_l.unlisten()
    assert ["b"] == c_1
