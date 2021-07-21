from pathlib import Path
import sys
from threading import Thread
from time import sleep
import tracemalloc as tm

sys.path.append(str(Path(__file__).parent.parent))
from sodiumfrp.core import Cell, StreamSink

def main() -> None:

    def thread() -> None:
        tm.start()
        while True:
            current, peak = tm.get_traced_memory()
            print(f"memory current: {current}, peak: {peak}")
            sleep(5)
    Thread(target=thread).start()

    et: StreamSink[int] = StreamSink()
    t = et.hold(0)
    etens = et.map(lambda x: x // 10)
    changeTens = et \
        .snapshot(t, lambda neu, old: None if neu == old else neu) \
        .filter(lambda x: x is not None)
    oout = changeTens \
        .map(lambda tens: t.map(lambda tt: (tens, tt))) \
        .hold(t.map(lambda tt: (0, tt)))
    out = Cell.switch_cell(oout)
    l = out.listen(lambda tu: None)
    for i in range(0, 1000000000):
        et.send(i)
    l.unlisten()

if __name__ == "__main__":
    main()
