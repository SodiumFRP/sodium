from typing import Callable, Optional, TypeVar

T = TypeVar("T")

class Transaction: ...

Handler = Callable[[Optional[T]], None]
TransactionHandler = Callable[[Transaction, T], None]
