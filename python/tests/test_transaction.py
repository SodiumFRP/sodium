from time import sleep
from threading import Thread
from typing import List

from sodiumfrp import Transaction

def test_post() -> None:
    out: List[int] = []
    Transaction.post(lambda: out.append(42))
    assert [42] == out

def test_post_overlapping() -> None:
    out: List[int] = []
    def thread() -> None:
        sleep(0.02)
        Transaction.post(lambda: out.append(42))
    Thread(target=thread).start()
    Transaction.run(lambda: sleep(0.04))
    assert [42] == out
