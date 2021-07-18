from typing import Callable, Generic, TypeVar

A = TypeVar("A")
B = TypeVar("B")

class Lazy(Generic[A]):
    """
    A representation for a value that may not be available until the current
    transaction is closed.
    """

    def __init__(self, f: Callable[[], A]) -> None:
        self._f = f


    def get(self) -> A:
        """
        Get the value if available, throwing an exception if not.
        In the general case this should only be used in subsequent
        transactions to when the Lazy was obtained.
        """
        return self._f()


    def lift(self, f: Callable[...,B], *others: "Lazy") -> "Lazy[B]":
        """
        Lift a function into lazy values, so the returned Lazy reflects
        the value of the function applied to the input Lazys' values.
        """
        def helper() -> B:
            other_values = map(lambda x: x.get(), others)
            return f(self.get(), *other_values)
        return Lazy(helper)
