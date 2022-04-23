from typing import Callable

class Listener:
    """
    A handle for a listener that was registered with :meth:`Cell.listen()
    <sodiumfrp.primitives.Cell.listen>` or :meth:`Stream.listen()
    <sodiumfrp.primitives.Stream.listen>`.
    """

    def __init__(self, unlisten: Callable[["Listener"], None]) -> None:
        self._unlisten = unlisten

    def unlisten(self) -> None:
        """
        Deregister the listener that was registered so it will no longer
        be called back, allowing associated resources to be garbage
        collected.
        """
        self._unlisten(self)

    def append(self, two: "Listener") -> "Listener":
        """
        Combine listeners into one so that invoking :meth:`unlisten` on
        the returned listener will unlisten both the inputs.
        """
        one = self
        def unlisten(_: "Listener") -> None:
            one.unlisten()
            two.unlisten()
        return Listener(unlisten)
