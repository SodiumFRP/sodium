from pathlib import Path
import sys
from threading import Thread
from time import sleep
import tracemalloc as tm

sys.path.append(str(Path(__file__).parent.parent))
from sodiumfrp import Cell, StreamSink

def main() -> None:

    def thread() -> None:
        tm.start()
        while True:
            current, peak = tm.get_traced_memory()
            print(f"memory current: {current}, peak: {peak}")
            sleep(5)
    Thread(target=thread).start()

    et: StreamSink[int] = StreamSink()
    eChange: StreamSink[int] = StreamSink()
    oout = eChange.map(lambda x: et).hold(et)
    out = Cell.switch_stream(oout)
    l = out.listen(lambda tt: None)
    for i in range(0, 1000000000):
        eChange.send(i)
    l.unlisten()

if __name__ == "__main__":
    main()
