from typing import List, Set
import weakref

from sodiumfrp.typing import TransactionHandler
from sodiumfrp.unit import Unit


class Target:

    def __init__(self, action: TransactionHandler[Unit], node: "Node") -> None:
        self.action = weakref.ref(action)
        self.node = node


class Node:

    def __init__(self, rank: int) -> None:
        self._rank = rank
        self._listeners: List[Target] = []


    def _link_to(self,
            action: TransactionHandler[Unit],
            target: "Node",
            out_target: List[Target]) -> bool:
        """ Return true if any changes were made. """
        changed = target._ensure_bigger_than(self._rank, set())
        t = Target(action, target)
        self._listeners.append(t)
        out_target[0] = t
        return changed


    def _unlink_to(self, target: Target) -> None:
        self._listeners.remove(target)


    def _ensure_bigger_than(self, limit: int, visited: Set["Node"]) -> bool:
        if self._rank > limit or (self in visited):
            return False

        visited.add(self)
        self._rank = limit + 1
        for listener in self._listeners:
            listener.node._ensure_bigger_than(self._rank, visited)
        visited.remove(self)
        return True


    def __lt__(self, other: "Node") -> bool:
        return self._rank < other._rank


    def __gt__(self, other: "Node") -> bool:
        return self._rank == other._rank
