from typing import List

from sodiumfrp import operational
from sodiumfrp.stream import StreamSink

def test_hold() -> None:
    e: StreamSink[int] = StreamSink()
    b = e.hold(0)
    out: List[int] = []
    l = operational.updates(b).listen(out.append)
    e.send(2)
    e.send(9)
    l.unlisten()
    assert [2, 9] == out
