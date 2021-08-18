""" Utilities for working with continuous time. """

from dataclasses import dataclass
from threading import Condition, Event, Thread
import time
from typing import Callable, List, Optional, Set, Tuple, TypeVar

from sodiumfrp import Cell, CellSink, Stream, StreamSink, Transaction

A = TypeVar("A")
B = TypeVar("B")

@dataclass
class Timer:
    time: int
    streams: Set[StreamSink[int]]

class TimerSystem:

    def __init__(self) -> None:
        self._time_ms: CellSink[int] = CellSink(int(_now_ms()))
        self._timers: List[Timer] = []
        self._timers_changed = Condition()
        self._stop = Event()
        self._released = False

        worker = Thread(target=self._thread)
        worker.daemon = True
        worker.start()

        self._unregister_hook = Transaction.on_start(
            self._on_transaction_start)

    def __del__(self) -> None:
        if not self._released:
            self.release()

    def __enter__(self) -> "TimerSystem":
        return self

    def __exit__(self, *args) -> None:
        self.release()

    def release(self) -> None:
        self._released = True
        self._stop.set()
        self._unregister_hook()
        with self._timers_changed:
            # Unlock the thread and let it exit the infinite loop
            self._timers_changed.notify_all()

    def _thread(self) -> None:
        """
        Timers are executed at the beginning of a transaction. If there are
        not transactions, no timers will be executed. Thus, we need
        a separate thread that would generate transactions firing timers.
        """
        while not self._stop.is_set():
            with self._timers_changed:
                timeout = None
                while self._timers:
                    earliest_timer = self._timers[0]
                    now = _now_ms()
                    if earliest_timer.time > now:
                        timeout = (earliest_timer.time - now) * 0.001
                        break
                    else:
                        # If the time of the earliest timer has passed,
                        # start an empty transaction to trigger all passed
                        # timers through the transaction start hook
                        Transaction.run(lambda: None)
                self._timers_changed.wait(timeout)

    def _on_transaction_start(self) -> None:
        """
        This should be used as a Transaction.on_start() hook. At the
        beginning of each transaction, update self._time_ms and execute
        all timers that should've been fired by this time.
        """
        trans_time = int(_now_ms())
        time_cell_updated = False
        with self._timers_changed:
            while self._timers:
                timer = self._timers[0]
                timer_time = timer.time
                if timer_time > trans_time:
                    break
                # Update time and execute timers inside the same transaction
                # to guarantee simultaneity of events
                def timer_transaction() -> None:
                    self._time_ms.send(timer_time)
                    for stream in timer.streams:
                        stream.send(timer_time)
                    del self._timers[0]
                    # Don't need to notify _thread(). It will wake up and
                    # skip to the next available timer on its own.
                Transaction.run(timer_transaction)
                if timer_time == trans_time:
                    time_cell_updated = True
            if not time_cell_updated:
                self._time_ms.send(trans_time)

    def time_ms(self) -> Cell[int]:
        """
        Returns a cell that represents current time (in milliseconds).
        """
        return self._time_ms

    def at(self, alarm_time: Cell[Optional[int]]) -> Stream[int]:
        """
        Returns a stream that fires events at the points in time indicated
        by the value of the input cell. If the value of the cell is None
        or less than the current time, then no events will be fired and
        the timer set by the previous value of the cell will be canceled.
        """
        alarm_stream: StreamSink[int] = StreamSink()
        current_alarm_time_value: List[Optional[int]] = [None]
        def handler(alarm_time_value: Optional[int]) -> None:
            if current_alarm_time_value[0] is not None:
                if alarm_time_value != current_alarm_time_value[0]:
                    self._cancel_timer(
                        current_alarm_time_value[0], alarm_stream)
            if alarm_time_value is not None:
                if alarm_time_value > self._time_ms.sample():
                    self._set_timer(alarm_time_value, alarm_stream)
                    current_alarm_time_value[0] = alarm_time_value
                else:
                    current_alarm_time_value[0] = None
            else:
                current_alarm_time_value[0] = None
        listener = alarm_time.listen(handler)
        return alarm_stream.add_cleanup(listener)

    def _cancel_timer(self, time_: int, stream: StreamSink[int]) -> None:
        with self._timers_changed:
            timer, index = _bisect(self._timers, time_, lambda t: t.time)
            if timer is not None:
                timer.streams.discard(stream)
                if len(timer.streams) == 0:
                    del self._timers[index]
                    self._timers_changed.notify_all()

    def _set_timer(self, time_: int, stream: StreamSink[int]) -> None:
        with self._timers_changed:
            timer, index = _bisect(self._timers, time_, lambda t: t.time)
            if timer is not None:
                timer.streams.add(stream)
            else:
                self._timers.insert(index, Timer(time_, {stream}))
                self._timers_changed.notify_all()

def _now_ms() -> float:
    return time.monotonic() * 1000

def _bisect(
        a: List[A],
        x: B, key: Callable[[A],B]) -> Tuple[Optional[A], int]:
    length = len(a)
    lo = 0
    hi = length
    while lo < hi:
        mid = (lo + hi) // 2
        if key(a[mid]) < x:
            lo = mid + 1
        else:
            hi = mid
    index = lo
    if index < length and key(a[index]) == x:
        return a[index], index
    else:
        return None, index
