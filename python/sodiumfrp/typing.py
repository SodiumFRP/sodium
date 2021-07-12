from typing import TYPE_CHECKING, Callable, TypeVar
if TYPE_CHECKING:
    from sodiumfrp.transaction import Transaction

T = TypeVar("T")

Handler = Callable[[T], None]
TransactionHandler = Callable[["Transaction", T], None]
