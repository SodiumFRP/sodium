from heapq import heappush, heappop
from threading import RLock
from typing import Callable, Dict, List, Optional, TypeVar

from sodiumfrp.node import Node
from sodiumfrp.typing import Handler, TransactionHandler

T = TypeVar("T")


class Entry:

    _next_seq: int = 0

    def __init__(self,
            rank: Node,
            action: Handler["Transaction"]) -> None:
        self.rank = rank
        self.action = action
        self.seq = Entry._next_seq
        Entry._next_seq += 1

    def __lt__(self, other: "Entry") -> bool:
        if self.rank < other.rank:
            return True
        elif self.rank > other.rank:
            return False
        # Same rank: preserve chronological sequence.
        return self.seq < other.seq


class Transaction:
    """ Functions for controlling transactions. """

    # Coarse-grained lock that's held during the whole transaction.
    _transaction_lock: RLock = RLock()
    _listeners_lock: RLock = RLock()
    _current_transaction: "Transaction" = None
    in_callback: int = 0
    _on_start_hooks: List[Callable[[], None]] = []
    _running_on_start_hooks: bool = False
    _post_q_lock = RLock()

    def __init__(self) -> None:
        # True if we need to re-generate the priority queue.
        self._to_regen = False
        self._prioritized_q: List[Entry] = []
        self._last_q: List[Callable[[], None]] = []
        self._post_q: Dict[int, Handler["Transaction"]] = None
        self._closed = False

    @staticmethod
    def get_current_transaction() -> "Transaction":
        """ Return the current transaction, or `None` if there isn't one. """
        with Transaction._transaction_lock:
            return Transaction._current_transaction

    @staticmethod
    def run(code: Callable[[], T]) -> T:
        """
        Run the specified code inside a single transaction, with
        the contained code returning a value of the parameter type `T`.

        In most cases this is not needed, because the primitives always
        create their own transaction automatically, but it is needed in some
        circumstances.
        """
        with Transaction._transaction_lock:
            # If we are already inside a transaction (which must be on
            # the same thread otherwise we wouldn't have acquired
            # transactionLock), then keep using that same transaction.
            prev_trans = Transaction._current_transaction
            try:
                Transaction._start_if_necessary()
                return code()
            finally:
                try:
                    if prev_trans is None:
                        Transaction._current_transaction.close()
                finally:
                    Transaction._current_transaction = prev_trans

    @staticmethod
    def _run_handler(code: Handler["Transaction"]) -> None:
        with Transaction._transaction_lock:
            # If we are already inside a transaction (which must be on
            # the same thread otherwise we wouldn't have acquired
            # transactionLock), then keep using that same transaction.
            prev_trans = Transaction._current_transaction
            try:
                Transaction._start_if_necessary()
                code(Transaction._current_transaction)
            finally:
                try:
                    if prev_trans is None:
                        Transaction._current_transaction.close()
                finally:
                    Transaction._current_transaction = prev_trans

    @staticmethod
    def on_start(runnable: Callable[[], None]) -> Callable[[], None]:
        """
        Add a runnable that will be executed whenever a transaction is
        started. That runnable may start transactions itself, which will
        not cause the hooks to be run recursively.

        The main use case of this is the implementation of a time/alarm
        system.

        Returns a callable which, when called, unregisters the hook.
        """
        with Transaction._transaction_lock:
            Transaction._on_start_hooks.append(runnable)
            def unregister_hook() -> None:
                with Transaction._transaction_lock:
                    Transaction._on_start_hooks.remove(runnable)
            return unregister_hook

    @staticmethod
    def _apply(code: Callable[["Transaction"], T]) -> T:
        with Transaction._transaction_lock:
            # If we are already inside a transaction (which must be on
            # the same thread otherwise we wouldn't have acquired
            # transactionLock), then keep using that same transaction.
            prev_trans = Transaction._current_transaction
            try:
                Transaction._start_if_necessary()
                return code(Transaction._current_transaction)
            finally:
                try:
                    if prev_trans is None:
                        Transaction._current_transaction.close()
                finally:
                    Transaction._current_transaction = prev_trans

    @staticmethod
    def _start_if_necessary() -> None:
        if Transaction._current_transaction is None:
            if not Transaction._running_on_start_hooks:
                Transaction._running_on_start_hooks = True
                try:
                    for hook in Transaction._on_start_hooks:
                        hook()
                finally:
                    Transaction._running_on_start_hooks = False
            Transaction._current_transaction = Transaction()

    def _prioritized(self, rank: Node, action: Handler["Transaction"]) -> None:
        entry = Entry(rank, action)
        heappush(self._prioritized_q, entry)

    def last(self, action: Callable[[], None]) -> None:
        """ Add an action to run after all :meth:`_prioritized` actions. """
        self._last_q.append(action)

    def _post(self, child_idx: int, action: Handler["Transaction"]) -> None:
        """ Add an action to run after all :meth:`last` actions. """
        if self._post_q is None:
            self._post_q = {}
        # If an entry exists already, combine the old one with the new one.
        if child_idx in self._post_q:
            existing = self._post_q[child_idx]
            def new(trans: Transaction) -> None:
                existing(trans)
                action(trans)
            self._post_q[child_idx] = new
        else:
            self._post_q[child_idx] = action

    @staticmethod
    def post(action: Callable[[], None]) -> None:
        """
        Execute the specified code after the current transaction is closed,
        or immediately if there is no current transaction.
        """
        if Transaction._current_transaction is not None:
            with Transaction._post_q_lock:
                if not Transaction._current_transaction._closed:
                    Transaction._current_transaction._post(
                        -1, lambda _: action())
                    return
        Transaction.run(action)

    def _check_regen(self) -> None:
        """
        If the priority queue has entries in it when we modify any of
        the nodes' ranks, then we need to re-generate it to make sure
        it's up-to-date.
        """
        if self._to_regen:
            self._to_regen = False
            self._prioritized_q.sort()

    def close(self) -> None:
        """ Close this transaction. Runs all scheduled actions. """

        while True:
            self._check_regen()
            if len(self._prioritized_q) == 0:
                break
            entry = heappop(self._prioritized_q)
            entry.action(self)

        for action in self._last_q:
            action()
        self._last_q.clear()

        with Transaction._post_q_lock:
            self._closed = True
            if self._post_q is not None:
                # _post_q must be traversed in the order of the indices
                for index in sorted(self._post_q.keys()):
                    handler = self._post_q.pop(index)
                    parent_trans = Transaction._current_transaction
                    try:
                        if index >= 0:
                            trans = Transaction()
                            Transaction._current_transaction = trans
                            try:
                                handler(trans)
                            finally:
                                trans.close()
                        else:
                            Transaction._current_transaction = None
                            handler(None)
                    finally:
                        Transaction._current_transaction = parent_trans
