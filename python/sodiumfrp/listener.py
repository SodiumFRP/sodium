class Listener:
    """
    A handle for a listener that was registered with Cell.listen() or
    Stream.listen()
    """

    def __init__(self) -> None:
        pass


    def unlisten(self) -> None:
        """
        Deregister the listener that was registered so it will no longer
        be called back, allowing associated resources to be garbage
        collected.
        """
        raise RuntimeError("Not implemented")


    def append(self, two: "Listener") -> "Listener":
        """
        Combine listeners into one so that invoking .unlisten() on
        the returned listener will unlisten both the inputs.
        """
        return ChainedListener(self, two)


class ChainedListener(Listener):

    def __init__(self, one: Listener, two: Listener):
        self.one = one
        self.two = two


    def unlisten(self) -> None:
        self.one.unlisten()
        self.two.unlisten()
