from typing import List

from sodiumfrp.stream import CellSink, Stream, StreamLoop, StreamSink
from sodiumfrp.transaction import Transaction

def test_send_stream() -> None:
    e: StreamSink[int] = StreamSink()
    out: List[int] = []
    l = e.listen(out.append)
    e.send(5)
    l.unlisten()
    assert [5] == out
    e.send(6)
    assert [5] == out

def test_map() -> None:
    e: StreamSink[int] = StreamSink()
    m = e.map(str)
    out: List[str] = []
    l = m.listen(out.append)
    e.send(5)
    l.unlisten()
    assert ["5"] == out

def test_map_to() -> None:
    e: StreamSink[int] = StreamSink()
    m = e.map_to("fusebox")
    out: List[str] = []
    l = m.listen(out.append)
    e.send(5)
    e.send(6)
    l.unlisten()
    assert ["fusebox", "fusebox"] == out

def test_merge_non_simultaneous() -> None:
    e1: StreamSink[int] = StreamSink()
    e2: StreamSink[int] = StreamSink()
    out: List[int] = []
    l = e2.or_else(e1).listen(out.append)
    e1.send(7)
    e2.send(9)
    e1.send(8)
    l.unlisten()
    assert [7, 9, 8] == out

def test_merge_simultaneous() -> None:
    s1: StreamSink[int] = StreamSink(lambda l, r: r)
    s2: StreamSink[int] = StreamSink(lambda l, r: r)
    out: List[int] = []
    l = s2.or_else(s1).listen(out.append)

    def trans1() -> None:
        s1.send(7)
        s2.send(60)
    Transaction.run(trans1)

    def trans2() -> None:
        s1.send(9)
    Transaction.run(trans2)

    def trans3() -> None:
        s1.send(7)
        s1.send(60)
        s2.send(8)
        s2.send(90)
    Transaction.run(trans3)

    def trans4() -> None:
        s2.send(8)
        s2.send(90)
        s1.send(7)
        s1.send(60)
    Transaction.run(trans4)

    def trans5() -> None:
        s2.send(8)
        s1.send(7)
        s2.send(90)
        s1.send(60)
    Transaction.run(trans5)

    l.unlisten()
    assert [60, 9, 90, 90, 90] == out

def test_coalesce() -> None:
    s: StreamSink[int] = StreamSink(lambda a, b: a + b)
    out: List[int] = []
    l = s.listen(out.append)

    def trans1() -> None:
        s.send(2)
    Transaction.run(trans1)

    def trans2() -> None:
        s.send(8)
        s.send(40)
    Transaction.run(trans2)

    l.unlisten()
    assert [2, 48] == out

def test_merge_multiple() -> None:
    s1: StreamSink[int] = StreamSink()
    s2: StreamSink[int] = StreamSink()
    s3: StreamSink[int] = StreamSink()
    out: List[int] = []
    l = Stream.merge_(lambda a, b: a + b, s1, s2, s3).listen(out.append)

    def trans1() -> None:
        s3.send(7)
        s1.send(1)
    Transaction.run(trans1)

    def trans2() -> None:
        s1.send(9)
    Transaction.run(trans2)

    def trans3() -> None:
        s2.send(6)
        s3.send(3)
        s1.send(2)
    Transaction.run(trans3)

    def trans4() -> None:
        s2.send(9)
        s1.send(1)
    Transaction.run(trans4)

    def trans5() -> None:
        s3.send(5)
        s1.send(9)
        s2.send(3)
    Transaction.run(trans5)

    l.unlisten()
    assert [8, 9, 11, 10, 17] == out

def test_or_else_multiple() -> None:
    s1: StreamSink[int] = StreamSink()
    s2: StreamSink[int] = StreamSink()
    s3: StreamSink[int] = StreamSink()
    out: List[int] = []
    l = Stream.or_else_(s1, s2, s3).listen(out.append)

    def trans1() -> None:
        s3.send(7)
        s1.send(1)
    Transaction.run(trans1)

    def trans2() -> None:
        s3.send(9)
    Transaction.run(trans2)

    def trans3() -> None:
        s2.send(6)
        s3.send(3)
        s1.send(2)
    Transaction.run(trans3)

    def trans4() -> None:
        s2.send(9)
        s1.send(1)
    Transaction.run(trans4)

    def trans5() -> None:
        s3.send(5)
        s1.send(9)
        s2.send(3)
    Transaction.run(trans5)

    l.unlisten()
    assert [1, 9, 2, 1, 9] == out

def test_filter() -> None:
    e: StreamSink[str] = StreamSink()
    out: List[str] = []
    l = e.filter(str.isupper).listen(out.append)
    e.send("H")
    e.send("o")
    e.send("I")
    l.unlisten()
    assert ["H", "I"] == out

def test_gate() -> None:
    ec: StreamSink[str] = StreamSink()
    epred = CellSink(True)
    out: List[str] = []
    l = ec.gate(epred).listen(out.append)
    ec.send("H")
    epred.send(False)
    ec.send("O")
    epred.send(True)
    ec.send("I")
    l.unlisten()
    assert ["H", "I"] == out

def test_loop_stream() -> None:
    ea: StreamSink[int] = StreamSink()
    def transaction() -> Stream[int]:
        eb: StreamLoop[int] = StreamLoop()
        ec_ = ea.map(lambda x: x % 10).merge(eb, lambda x, y: x + y)
        eb_out = ea.map(lambda x: x // 10).filter(lambda x: x != 0)
        eb.loop(eb_out)
        return ec_
    ec = Transaction.run(transaction)
    out: List[int] = []
    l = ec.listen(out.append)
    ea.send(2)
    ea.send(52)
    l.unlisten()
    assert [2, 7] == out
