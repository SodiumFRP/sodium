""" Operational primitives that must be used with care. """

from typing import Iterable, TypeVar

from sodiumfrp.primitives import Cell, Stream, StreamWithSend
from sodiumfrp.transaction import Transaction

A = TypeVar("A")

def updates(c: Cell[A]) -> Stream[A]:
    """
    A stream that gives the updates/steps for a `Cell`.

    This is an OPERATIONAL primitive, which is not part of the main Sodium
    API. It breaks the property of non-detectability of cell steps/updates.
    The rule with this primitive is that you should only use it in functions
    that do not allow the caller to detect the cell updates.
    """
    return c._updates()


def value(c: Cell[A]) -> Stream[A]:
    """
    A stream that is guaranteed to fire once in the transaction where
    value() is invoked, giving the current value of the cell, and thereafter
    behaves like `updates()`, firing for each update/step of the cell's value.

    This is an OPERATIONAL primitive, which is not part of the main Sodium
    API. It breaks the property of non-detectability of cell steps/updates.
    The rule with this primitive is that you should only use it in functions
    that do not allow the caller to detect the cell updates.
    """
    return Transaction._apply(c._value_stream)


def defer(s: Stream[A]) -> Stream[A]:
    """
    Push each event onto a new transaction guaranteed to come before
    the next externally initiated transaction. Same as `split()` but it
    works on a single value.
    """
    return split(s.map(lambda a: [a]))


def split(s: Stream[Iterable[A]]) -> Stream[A]:
    """
    Push each event in the list onto a newly created transaction guaranteed
    to come before the next externally initiated transaction. Note that
    the semantics are such that two different invocations of split() can
    put events into the same new transaction, so the resulting stream's
    events could be simultaneous with events output by split() or `defer()`
    invoked elsewhere in the code.
    """
    out: StreamWithSend[A] = StreamWithSend()

    def handler(trans: Transaction, as_: Iterable[A]) -> None:
        child_idx = 0
        for a in as_:
            def run(trans2: Transaction, _a: A = a) -> None:
                out._send(trans2, _a)
            trans._post(child_idx, run)
            child_idx += 1

    l1 = s._listen(out._node, handler)
    return out._unsafe_add_cleanup(l1)
