from time import sleep, monotonic
from typing import List, Optional

from sodiumfrp import Cell, CellLoop, CellSink, Listener, Stream, Transaction
from sodiumfrp.time import TimerSystem

def test_time_sample() -> None:
    with TimerSystem() as ts:
        t1 = Transaction.run(ts.time_ms().sample)
        sleep(0.1)
        t2 = Transaction.run(ts.time_ms().sample)
        delta = t2 - t1
        # This test may fail occasionally, because there is no way to measure
        # time precisely
        assert abs(delta - 100) < 5

def test_at() -> None:
    with TimerSystem() as ts:
        time_ = Transaction.run(ts.time_ms().sample)
        alarm = ts.at(Cell.constant(time_ + 100))
        out: List[int] = []
        alarm.listen(lambda t: out.append(t - time_))
        sleep(0.11)
        assert [100] == out

def test_merge_non_simultaneous() -> None:
    with TimerSystem() as ts:
        out: List[int] = []
        now = Transaction.run(ts.time_ms().sample)
        a1 = ts.at(Cell.constant(now + 100))
        a2 = ts.at(Cell.constant(now + 101))
        m = Stream.or_else_(
            a1.map_to(1),
            a2.map_to(2),
        )
        m.listen(out.append)
        sleep(0.11)
        assert [1, 2] == out

def test_merge_simultaneous() -> None:
    with TimerSystem() as ts:
        out: List[int] = []
        now = Transaction.run(ts.time_ms().sample)
        a1 = ts.at(Cell.constant(now + 100))
        a2 = ts.at(Cell.constant(now + 100))
        m = a1.or_else(a2)
        m.listen(out.append)
        sleep(0.11)
        assert 1 == len(out)

def test_periodic() -> None:
    with TimerSystem() as ts:
        errors: List[float] = []

        def transaction() -> None:
            t0 = ts.time_ms().sample()

            def update_alarm(t: int) -> Optional[int]:
                if ts.time_ms().sample() < (t0 + 110):
                    return t + 1
                return None

            alarm_time: CellLoop[Optional[int]] = CellLoop()
            alarm = ts.at(alarm_time)
            alarm_time.loop(alarm.map(update_alarm).hold(t0 + 10))

            def error(t: int) -> float:
                return abs(monotonic() * 1000 - t)
            alarm.listen(lambda t: errors.append(error(t)))

        Transaction.run(transaction)

        sleep(0.12)

        assert len(errors) == 102
        rmse = pow(sum(map(lambda x: x * x, errors)) / len(errors), 0.5)
        # This test may fail occasionally, because there is no way to measure
        # time precisely
        assert rmse < 1.0

def test_passed_time() -> None:
    with TimerSystem() as ts:
        out: List[int] = []
        def transaction() -> Listener:
            now = ts.time_ms().sample()
            alarm = ts.at(Cell.constant(now))
            return alarm.listen(out.append)
        l = Transaction.run(transaction)
        sleep(0.1)
        l.unlisten()
        assert len(out) == 0

def test_cancel_alarm() -> None:
    with TimerSystem() as ts:
        alarm_time: CellSink[Optional[int]] = CellSink(None)
        out: List[int] = []
        def transaction() -> Listener:
            now = ts.time_ms().sample()
            alarm_time.send(now + 100)
            alarm = ts.at(alarm_time)
            return alarm.listen(out.append)
        l = Transaction.run(transaction)
        sleep(0.055)
        alarm_time.send(None)
        sleep(0.055)
        l.unlisten()
        assert len(out) == 0
