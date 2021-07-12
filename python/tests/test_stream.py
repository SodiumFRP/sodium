from typing import List

from sodiumfrp.stream import StreamSink

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
