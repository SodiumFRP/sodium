from dataclasses import dataclass
from threading import RLock
import traceback
from typing import \
    Any, \
    Callable, \
    Generic, \
    Iterable, \
    List, \
    Optional, \
    Sequence, \
    Set, \
    Tuple, \
    TypeVar, \
    Union
import weakref

from sodiumfrp.lazy import Lazy
from sodiumfrp.listener import Listener
from sodiumfrp.node import NODE_NULL, Node, Target
from sodiumfrp.transaction import Transaction
from sodiumfrp.typing import Handler, TransactionHandler
from sodiumfrp.unit import Unit, UNIT

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")
S = TypeVar("S")
T = TypeVar("T")


class Stream(Generic[A]):
    """
    Represents a stream of discrete events/firings containing values of
    type `A`.
    """

    _keep_listeners_alive: Set[Listener] = set()
    _keep_listeners_alive_lock: RLock = RLock()

    def __init__(self,
            node: Node,
            finalizers: List[Listener],
            firings: List[A]) -> None:
        self._node = node
        self._finalizers = finalizers
        self._firings = firings

    @staticmethod
    def never() -> "StreamWithSend[A]":
        """ A stream that never fires. """
        return StreamWithSend()

    def listen(self, handler: Handler[A]) -> Listener:
        """
        Listen for events/firings on this stream. This is the observer
        pattern. The returned :class:`~sodiumfrp.listener.Listener` has an
        :meth:`~sodiumfrp.listener.Listener.unlisten` method
        to cause the listener to be removed. This is an **operational**
        mechanism is for interfacing between the world of I/O and for FRP.

        :param handler: The handler to execute when there's a new value.
            You should make no assumptions about what thread you are
            called on, and the handler should not block. You are not allowed
            to use :meth:`CellSink.send` or :meth:`StreamSink.send` in
            the handler. An exception will be thrown, because you are not
            meant to use this to create your own primitives.
        """
        l0 = self.listen_weak(handler)
        def unlisten(_self: Listener) -> None:
            l0.unlisten()
            with Stream._keep_listeners_alive_lock:
                Stream._keep_listeners_alive.remove(_self)
        l = Listener(unlisten)
        with Stream._keep_listeners_alive_lock:
            Stream._keep_listeners_alive.add(l)
        return l

    def _listen(self,
            target: Node,
            action: TransactionHandler[A]) -> Listener:
        return Transaction._apply(
            lambda trans1: self._listen_internal(
                target, trans1, action, False))

    def listen_weak(self, action: Handler[A]) -> Listener:
        """
        A variant of :meth:`listen` that will deregister the listener
        automatically if the listener is garbage collected. With
        :meth:`listen`, the listener is only deregistered if
        :meth:`Listener.unlisten() <sodiumfrp.listener.Listener.unlisten>`
        is called explicitly.

        This method should be used for listeners that are to be passed to
        :meth:`Stream.add_cleanup` to ensure that things don't get kept
        alive when they shouldn't.
        """
        return self._listen(NODE_NULL, lambda trans2, a: action(a))

    def _listen_internal(self,
            target: Node,
            trans: Transaction,
            action: TransactionHandler[A],
            suppress_earlier_firings: bool) -> Listener:
        node_target_: List[Target] = [None]
        with Transaction._listeners_lock:
            if self._node._link_to(action, target, node_target_):
                trans._to_regen = True
        node_target = node_target_[0]
        firings = self._firings.copy()
        if (not suppress_earlier_firings) and (len(firings) > 0):
            def handler(trans2: Optional[Transaction]) -> None:
                for firing in firings:
                    Transaction.in_callback += 1
                    try:
                        action(trans2, firing)
                    except:
                        # Don't allow transactions to interfere with Sodium
                        # internals.
                        _print_stack()
                    finally:
                        Transaction.in_callback -= 1
            trans._prioritized(target, handler)

        captures: List[Any] = [
            # It's essential that we keep the listener alive while
            # the caller holds the Listener, so that the finalizer doesn't
            # get triggered.
            self,
            # It's also essential that we keep the action alive, since
            # the node uses a weak reference.
            action,
            node_target
        ]

        def unlisten(_self: Listener) -> None:
            event, _, target = captures
            with Transaction._listeners_lock:
                if event is not None:
                    event._node._unlink_to(target)
                    # Release captured references
                    captures[0] = None
                    captures[1] = None
                    captures[2] = None

        return Listener(unlisten)

    def map(self, f: Callable[[A], B]) -> "Stream[B]":
        """
        Transform the stream's event values according to the supplied
        function, so the returned Stream's event values reflect the value
        of the function applied to the input Stream's event values.

        :param f: Function to apply to convert the values. It may construct
            FRP logic or use :meth:`Cell.sample` in which case
            it is equivalent to :meth:`Stream.snapshot`'ing the cell. Apart
            from this the function must be **referentially transparent**.
        """
        out: StreamWithSend[B] = StreamWithSend()
        l = self._listen(out._node,
            lambda trans2, a: out._send(trans2, f(a)))
        return out._unsafe_add_cleanup(l)

    def map_to(self, b: B) -> "Stream[B]":
        """
        Returns a stream that outputs `b` for each event of the source
        stream.
        """
        return self.map(lambda _: b)

    def starmap(self, f: Callable[...,T]) -> "Stream[T]":
        """
        Like :meth:`Stream.map`, except that the elements of the stream are
        iterables that are unpacked as arguments when passing to the mapping
        function.
        """
        return self.map(lambda t: f(*t))

    def hold(self, init_value: A) -> "Cell[A]":
        """
        Create a cell with the specified initial value, that is updated
        by this stream's event values.

        There is an implicit delay: state updates caused by event firings
        don't become visible as the cell's current value as viewed by
        :meth:`Stream.snapshot` until the following transaction.
        To put this another way, :meth:`Stream.snapshot` always sees
        the value of a cell as it was before any state changes from
        the current transaction.
        """
        assert isinstance(self, StreamWithSend)
        return Transaction._apply(
            lambda trans: Cell(self, init_value))

    def hold_lazy(self, init_value: Lazy[A]) -> "Cell[A]":
        """
        A variant of :meth:`hold` with an initial value captured
        by :meth:`Cell.sample_lazy`.
        """
        return Transaction._apply(
            lambda trans: self._hold_lazy(trans, init_value))

    def _hold_lazy(self, _: Transaction, init_value: Lazy[A]) -> "Cell[A]":
        return LazyCell(self, init_value)

    def snapshot(self, *args: Union["Cell", Callable[...,T]]) -> "Stream[T]":
        """
        Return a stream whose events are the result of the combination, using
        the specified function, of the input stream's event value and
        the values of the provided cells at the time of the event.

        The comibining function is optional and, if it is present, must be
        provided as the last argument. If the function is omitted,
        the default combining function will be used, which ignores
        the stream's value and returns the values of the provided cells
        as a tuple. If only one cell provided, the default combining function
        returns just the value of this cell.

        There is an implicit delay: state updates caused by event firings
        being held with :meth:`Stream.hold` don't become visible as the cell's
        current value until the following transaction. To put this another
        way, :meth:`Stream.snapshot` always sees the value of a cell as it
        was before any state changes from the current transaction.
        """

        def parse_args() -> Tuple[Tuple, Callable]:
            if len(args) == 0:
                raise RuntimeError(
                    "Stream.snapshot() expects one or more arguments")

            def check_cell_types(cells: Iterable[Cell]) -> None:
                for index, cell in enumerate(cells):
                    if not isinstance(cell, Cell):
                        raise TypeError(
                            f"Argument {index} must be of type Cell. "
                            f"Actual type: {type(cell)}")

            if callable(args[-1]):
                cells = args[:-1]
                if len(cells) == 0:
                    raise RuntimeError("No cells were provided")
                check_cell_types(cells)
                f = args[-1]
                combine = lambda a: \
                    f(a, *map(lambda c: c._sample_no_trans(), cells))
                return cells, combine
            else:
                cells = args
                check_cell_types(cells)
                if len(cells) == 1:
                    cell = cells[0]
                    combine = lambda a: cell._sample_no_trans()
                else:
                    combine = lambda a: \
                        tuple(map(lambda c: c._sample_no_trans(), cells))
                return cells, combine

        cells, combine = parse_args()

        out: StreamWithSend[T] = StreamWithSend()
        l = self._listen(out._node,
            lambda trans2, a: out._send(trans2, combine(a)))
        return out._unsafe_add_cleanup(l)

    def or_else(self, s: "Stream[A]") -> "Stream[A]":
        """
        Variant of :meth:`merge_with` that merges two streams and will drop
        an event in the simultaneous case.

        In the case where two events are simultaneous (i.e. both within
        the same transaction), the event from `this` will take precedence,
        and the event from `s` will be dropped. If you want to specify your
        own combining function, use :meth:`merge_with`. `s1.or_else(s2)` is
        equivalent to `s1.merge_with(s2, lambda l, r: l)`.

        The name `or_else` is used instead of `merge_with` to make it
        really clear that care should be taken, because events can be dropped.
        """
        return self.merge_with(s, lambda left, right: left)

    @staticmethod
    def _merge(ea: "Stream[A]", eb: "Stream[A]") -> "Stream[A]":
        out: StreamWithSend[A] = StreamWithSend()
        left = Node(0)
        right = out._node
        node_target_: List[Target] = [None]
        left._link_to(lambda _: None, right, node_target_)
        node_target = node_target_[0]
        h = out._send
        l1 = ea._listen(left, h)
        l2 = eb._listen(right, h)
        def unlisten(_self: Listener) -> None:
            left._unlink_to(node_target)
        return out \
            ._unsafe_add_cleanup(l1) \
            ._unsafe_add_cleanup(l2) \
            ._unsafe_add_cleanup(Listener(unlisten))

    def merge_with(self,
            s: "Stream[A]",
            f: Callable[[A, A], A]) -> "Stream[A]":
        """
        Merge two streams of the same type into one, so that events on
        either input appear on the returned stream.

        If the events are simultaneous (that is, one event from this and one
        from `s` occurring in the same transaction), combine them into one
        using the specified combining function so that the returned stream
        is guaranteed only ever to have one event per transaction. The event
        from `self` will appear at the left input of the combining
        function, and the event from `s` will appear at the right.

        :param f: Function to combine the values. It may construct FRP logic
            or use :meth:`Cell.sample`. Apart from this the function must be
            **referentially transparent**.
        """
        return Transaction._apply(
            lambda trans: Stream._merge(self, s)._coalesce(trans, f))

    @staticmethod
    def or_else_(*streams: "Stream[A]") -> "Stream[A]":
        """
        Variant of :meth:`or_else` that merges a collection of streams.
        """
        return Stream.merge(lambda left, right: left, *streams)

    @staticmethod
    def merge(
            f: Callable[[A,A],A],
            *streams: "Stream[A]") -> "Stream[A]":
        """
        Variant of :meth:`merge_with` that merges multiple streams.
        """
        return Stream._merge_many(streams, 0, len(streams), f)

    @staticmethod
    def _merge_many(
            streams: Sequence["Stream[A]"],
            start: int,
            end: int,
            f: Callable[[A,A],A]) -> "Stream[A]":
        length = end - start
        if length == 0:
            return Stream.never()
        elif length == 1:
            return streams[start]
        elif length == 2:
            left = streams[start]
            right = streams[start + 1]
            return left.merge_with(right, f)
        else:
            mid = (start + end) // 2
            left = Stream._merge_many(streams, start, mid, f)
            right = Stream._merge_many(streams, mid, end, f)
            return left.merge_with(right, f)

    def _coalesce(self,
            trans1: Transaction,
            f: Callable[[A,A],A]) -> "Stream[A]":
        out: StreamWithSend[A]  = StreamWithSend()
        h = CoalesceHandler(f, weakref.ref(out))
        l = self._listen_internal(out._node, trans1, h, False)
        return out._unsafe_add_cleanup(l)

    def _last_firing_only(self, trans: Transaction) -> "Stream[A]":
        """
        Clean up the output by discarding any firing other than the last one.
        """
        return self._coalesce(trans, lambda first, second: second)

    def filter(self, predicate: Callable[[A], bool]) -> "Stream[A]":
        """
        Return a stream that only outputs events for which the predicate
        returns `True`.
        """
        out: StreamWithSend[A] = StreamWithSend()
        def handler(trans2: Transaction, a: A) -> None:
            if predicate(a):
                out._send(trans2, a)
        l = self._listen(out._node, handler)
        return out._unsafe_add_cleanup(l)

    def gate(self, c: "Cell[bool]") -> "Stream[A]":
        """
        Return a stream that only outputs events from the input stream
        when the specified cell's value is `True`.
        """
        skip = object()
        return self \
            .snapshot(c, lambda a, pred: a if pred else skip) \
            .filter(lambda x: x is not skip)

    def collect(self,
            init_state: S,
            f: Callable[[A, S], Tuple[B, S]]) -> "Stream[B]":
        """
        Transform an event with a generalized state loop (a Mealy machine).
        The function is passed the input and the old state and returns
        the new state and output value.

        :param f: Function to apply to update the state. It may construct FRP
            logic or use :meth:`Cell.sample` in which case it is equivalent
            to :meth:`Stream.snapshot`'ing the cell. Apart from this
            the function must be **referentially transparent**.
        """
        return self.collect_lazy(Lazy(lambda: init_state), f)

    def collect_lazy(self,
            init_state: Lazy[S],
            f: Callable[[A, S], Tuple[B, S]]) -> "Stream[B]":
        """
        A variant of :meth:`collect` that takes an initial state returned by
        :meth:`Cell.sample_lazy`.
        """
        def handler() -> Stream[B]:
            ea = self
            es: StreamLoop[S] = StreamLoop()
            s = es.hold_lazy(init_state)
            ebs = ea.snapshot(s, f)
            eb = ebs.map(lambda bs: bs[0])
            es_out = ebs.map(lambda bs: bs[1])
            es.loop(es_out)
            return eb

        return Transaction.run(handler)

    def accum(self, init_state: S, f: Callable[[A, S], S]) -> "Cell[S]":
        """
        Accumulate on input event, outputting the new state each time.

        :param f: Function to apply to update the state. It may construct FRP
            logic or use :meth:`Cell.sample` in which case it is equivalent
            to :meth:`Stream.snapshot`'ing the cell. Apart from this
            the function must be **referentially transparent**.
        """
        return self.accum_lazy(Lazy(lambda: init_state), f)

    def accum_lazy(self,
            init_state: Lazy[S],
            f: Callable[[A, S], S]) -> "Cell[S]":
        """
        A variant of :meth:`accum` that takes an initial state returned by
        :meth:`Cell.sample_lazy`.
        """
        def handler() -> Cell[S]:
            ea = self
            es: StreamLoop[S] = StreamLoop()
            s = es.hold_lazy(init_state)
            es_out = ea.snapshot(s, f)
            es.loop(es_out)
            return es_out.hold_lazy(init_state)

        return Transaction.run(handler)

    def once(self) -> "Stream[A]":
        """
        Return a stream that outputs only one value: the next event of the
        input stream, starting from the transaction in which `once()` was
        invoked.
        """
        # This is a bit long-winded but it's efficient because it deregisters
        # the listener.
        la: List[Optional[Listener]] = [None]
        out: StreamWithSend[A] = StreamWithSend()

        def handler(trans: Transaction, a: A) -> None:
            if la[0] is not None:
                out._send(trans, a)
                la[0].unlisten()
                la[0] = None

        la[0] = self._listen(out._node, handler)
        return out._unsafe_add_cleanup(la[0])

    def calm(self) -> "Stream[A]":
        """
        Returns a stream that ignores repeating values produced
        by the source stream.
        """
        return self._calm(None)

    def _calm(self, init) -> "Stream[A]":
        reducer = lambda new, old: (None, old) if new == old else (new, new)
        return self.collect(init, reducer).filter(lambda x: x is not None)

    def _unsafe_add_cleanup(self, cleanup: Listener) -> "Stream[A]":
        """
        This is not thread-safe, so one of these two conditions must apply:
        1. We are within a transaction, since in the current implementation
           a transaction locks out all other threads.
        2. The object on which this is being called has not yet been
           returned from the method where it was created, so it can't
           be shared between threads.
        """
        self._finalizers.append(cleanup)
        return self

    def add_cleanup(self, cleanup: Listener) -> "Stream[A]":
        """
        Attach a listener to this stream so that its
        :meth:`Listener.unlisten() <sodiumfrp.listener.Listener.unlisten>`
        is invoked when this stream is garbage collected. Useful for
        functions that initiate I/O, returning the result of it through
        a stream.

        You must use this only with listeners returned
        by :meth:`listen_weak()` so that things don't get kept alive when
        they shouldn't.
        """
        def helper() -> Stream[A]:
            fs_new = self._finalizers.copy()
            fs_new.append(cleanup)
            return Stream(self._node, fs_new, self._firings)

        return Transaction.run(helper)

    def __del__(self) -> None:
        for l in self._finalizers:
            l.unlisten()


class StreamWithSend(Stream[A]):

    def __init__(self) -> None:
        super().__init__(Node(0), [], [])

    def _send(self, trans: Transaction, a: A) -> None:
        if len(self._firings) == 0:
            trans.last(lambda: self._firings.clear())
        self._firings.append(a)
        with Transaction._listeners_lock:
            listeners = set(self._node._listeners)
        for target in listeners:
            def handler(
                    trans2: Transaction,
                    # Pass target as default argument, as loop variables
                    # shouldn't be captured by a closure
                    target: Target = target) -> None:
                Transaction.in_callback += 1
                try:
                    uta = target.action() # Dereference the weak reference
                    if uta is not None: # If it hasn't been GC'ed, call it
                        uta(trans2, a)
                except:
                    # Don't allow transactions to interfere with Sodium
                    # internals.
                    _print_stack()
                finally:
                    Transaction.in_callback -= 1
            trans._prioritized(target.node, handler)


class StreamSink(StreamWithSend[A]):
    """
    A stream that allows values to be pushed into it, acting as an interface
    between the world of I/O and the world of FRP. Code that exports
    `StreamSinks` for read-only use should downcast to :class:`Stream`.
    """

    def __init__(self, f: Callable[[A, A], A] = None) -> None:
        """
        Construct a `StreamSink`. Use the provided function to combine values,
        that were sent to the stream during the same transaction, into a
        single event. If the function is `None`, :meth:`send` throws
        an exception, if it is called more then once per transaction.

        The combining function should be **associative**.

        :param f: Function to combine the values. It may construct FRP logic
            or use :meth:`Cell.sample`. Apart from this the function
            must be **referentially transparent**.
        """
        super().__init__()
        def error(left: A, right: A) -> A:
            raise RuntimeError("send() called more than once per "
                "transaction, which isn't allowed. Did you want to combine "
                "the events? Then pass a combining function to your "
                "StreamSink constructor.");
        self._coalescer = CoalesceHandler(
            f if f is not None else error, weakref.ref(self))

    def send(self, a: A) -> None:
        """
        Send a value to be made available to consumers of the stream.
        `send()` may not be used inside handlers registered with
        :meth:`Stream.listen` or :meth:`Cell.listen`. An exception
        will be thrown, because `StreamSink` is for interfacing I/O
        to FRP only. You are not meant to use this to define your own
        primitives.

        :param a: value to push into the cell.
        """
        def handler(trans: Transaction) -> None:
            if trans.in_callback > 0:
                raise RuntimeError("You are not allowed to use send() "
                    "inside a Sodium callback");
            self._coalescer(trans, a)
        Transaction._run_handler(handler)


class StreamLoop(StreamWithSend[A]):
    """
    A forward reference for a `Stream` equivalent to the `Stream` that
    is referenced.
    """

    def __init__(self) -> None:
        super().__init__()
        self._assigned = False
        if Transaction.get_current_transaction() is None:
            raise RuntimeError("StreamLoop/CellLoop must be used within "
                "an explicit transaction")

    def loop(self, ea_out: Stream[A]) -> None:
        """
        Resolve the loop to specify what the `StreamLoop` was a forward
        reference to. It must be invoked inside the same transaction as
        the place where the `StreamLoop` is used. This requires you to create
        an explicit transaction with :meth:`Transaction.run()
        <sodiumfrp.transaction.Transaction.run>`.
        """
        if self._assigned:
            raise RuntimeError("StreamLoop looped more than once")
        self._assigned = True
        Transaction.run(
            lambda: self._unsafe_add_cleanup(
                ea_out._listen(self._node, self._send)
            )
        )


class CoalesceHandler(Generic[A]):

    def __init__(self,
            f: Callable[[A,A], A],
            # Pass a weak reference here, to prevent circular references
            # in Stream._coalesce() and StreamSink.__init__().
            out: "weakref.ReferenceType[StreamWithSend[A]]") -> None:
        self._f = f
        self._out = out
        self._accum_valid = False
        self._accum: A = None

    def __call__(self, trans1: Transaction, a: A) -> None:
        if self._accum_valid:
            self._accum = self._f(self._accum, a)
        else:
            def handler(trans2: Optional[Transaction]) -> None:
                self._out()._send(trans2, self._accum)
                self._accum_valid = False
                self._accum = None
            trans1._prioritized(self._out()._node, handler)
            self._accum = a
            self._accum_valid = True


class Cell(Generic[A]):
    """ Represents a value of type `A` that changes over time. """

    @staticmethod
    def constant(value: A) -> "Cell[A]":
        """ Returns a cell that never changes, always holding the `value`. """
        return Cell(Stream.never(), value)

    def __init__(self, stream: StreamWithSend[A], init_value: A) -> None:
        self._stream = stream
        self._value = init_value
        self._value_update: A = None
        self._cleanup: Listener = None
        self._lazy_init_value: Lazy[A] = None # Use by LazyCell
        def handler(trans1: Transaction) -> None:
            def handler2(trans2: Transaction, a: A) -> None:
                if self._value_update is None:
                    def run() -> None:
                        self._value = self._value_update
                        self._lazy_init_value = None
                        self._value_update = None
                    trans2.last(run)
                self._value_update = a
            self._cleanup = stream._listen_internal(
                NODE_NULL, trans1, handler2, False)
        Transaction._run_handler(handler)

    def sample(self) -> A:
        """
        Sample the cell's current value.

        It may be used inside the functions passed to primitives that apply
        them to :class:`Stream` including :meth:`Stream.map` (in which case
        it is equivalent to snapshotting the cell), :meth:`Stream.snapshot`,
        :meth:`Stream.filter` and :meth:`Stream.merge`.

        It should generally be avoided in favour of :meth:`listen` so you
        don't miss any updates, but in many circumstances it makes sense.
        """
        return Transaction._apply(lambda trans: self._sample_no_trans())

    def sample_lazy(self) -> Lazy[A]:
        """
        A variant of :meth:`sample` that works with :class:`CellLoop` when
        they haven't been looped yet.  It should be used in any code that's
        general enough that it could be passed a :class:`CellLoop`.
        """
        return Transaction._apply(lambda trans: self._sample_lazy(trans))

    def _sample_lazy(self, trans: Transaction) -> Lazy[A]:
        @dataclass
        class LazySample:
            cell: Cell[A]
            has_value: bool
            value: A
        s = LazySample(cell=self, has_value=None, value=None)
        def handler() -> None:
            s.value = self._value_update \
                if self._value_update is not None \
                else self._sample_no_trans()
            s.has_value = True
            s.cell = None
        trans.last(handler)
        return Lazy(lambda: s.value if s.has_value else s.cell.sample())

    def _sample_no_trans(self) -> A:
        return self._value

    def _updates(self) -> Stream[A]:
        return self._stream

    def _value_stream(self, trans1: Transaction) -> Stream[A]:
        s_spark: StreamWithSend[Unit] = StreamWithSend()
        trans1._prioritized(
            s_spark._node, lambda trans2: s_spark._send(trans2, UNIT))
        s_initial: Stream[A] = s_spark.snapshot(self)
        return s_initial.merge_with(
            self._updates(), lambda left, right: right)

    def map(self, f: Callable[[A], B]) -> "Cell[B]":
        """
        Transform the cell's value according to the supplied function,
        so the returned cell always reflects the value of the function
        applied to the input cell's value.
        """
        return Transaction._apply(
            lambda trans: self \
                ._updates() \
                .map(f) \
                ._hold_lazy(trans, self._sample_lazy(trans).lift(f))
        )

    def starmap(self, f: Callable[...,T]) -> "Cell[T]":
        """
        Like :meth:`Cell.map`, except that the value of the cell is
        an iterable that is unpacked as arguments when passing to
        the mapping function.
        """
        return self.map(lambda t: f(*t))

    # TODO lift over multiple cells at once
    def lift(self, b: "Cell[B]", f: Callable[[A,B],C]) -> "Cell[C]":
        """
        Lift a binary function into cells, so the returned cell always
        reflects the specified function applied to the input cells' values.

        :param f: Function to apply. It must be **referentially transparent**.
        """
        bf = self.map(lambda aa: lambda bb: f(aa, bb))
        return Cell.apply(bf, b)

    @staticmethod
    def apply(bf: "Cell[Callable[[A],B]]", ba: "Cell[A]") -> "Cell[B]":
        """
        Apply a value inside a cell to a function inside a cell. This is the
        primitive for all function lifting.
        """
        def handler(trans0: Transaction) -> "Cell[B]":
            out: StreamWithSend[B] = StreamWithSend()

            class ApplyHandler:
                def __init__(self) -> None:
                    self.f: Callable[[A],B] = None
                    self.f_present = False
                    self.a: A = None
                    self.a_present = False
                def run(self, trans1: Transaction) -> None:
                    trans1._prioritized(out._node,
                        lambda trans2: out._send(trans2, self.f(self.a)))

            out_target = out._node
            in_target = Node(0)
            node_target_: List[Target] = [None]
            in_target._link_to(lambda _: None, out_target, node_target_)
            node_target = node_target_[0]
            h = ApplyHandler()

            def handler_f(trans1: Transaction, f: Callable[[A],B]) -> None:
                h.f = f
                h.f_present = True
                if h.a_present:
                    h.run(trans1)

            l1 = bf._value_stream(trans0)._listen(in_target, handler_f)

            def handler_a(trans1: Transaction, a: A) -> None:
                h.a = a
                h.a_present = True
                if h.f_present:
                    h.run(trans1)

            l2 = ba._value_stream(trans0)._listen(in_target, handler_a)

            return out \
                ._last_firing_only(trans0) \
                ._unsafe_add_cleanup(l1) \
                ._unsafe_add_cleanup(l2) \
                ._unsafe_add_cleanup(
                    Listener(lambda _: in_target._unlink_to(node_target))) \
                .hold_lazy(
                    Lazy(
                        lambda: bf._sample_no_trans()(ba._sample_no_trans())
                    )
                )

        return Transaction._apply(handler)

    def switch(self) -> "Cell":
        """
        Do either :meth:`switch_stream` or :meth:`switch_cell` depending on
        the type of the value of the cell.
        """
        if isinstance(self._value, Stream):
            return Cell.switch_stream(self)
        elif isinstance(self._value, Cell):
            return Cell.switch_cell(self)
        else:
            raise TypeError(
                "Can't apply Cell.switch() to a Cell holding value of type "
                f"{type(self._value)}. Type of the value must be either "
                "Stream or Cell.")

    @staticmethod
    def switch_cell(bba: "Cell[Cell[A]]") -> "Cell[A]":
        """
        Unwrap a cell inside another cell to give a time-varying cell
        implementation.
        """
        def helper(trans0: Transaction) -> "Cell[A]":
            za = bba.sample_lazy().lift(lambda ba: ba.sample())
            out: StreamWithSend[A] = StreamWithSend()

            class _Handler:

                def __init__(self) -> None:
                    self.current_listener: Listener = None

                def __call__(self, trans2: Transaction, ba: Cell[A]) -> None:
                    # Note: If any switch takes place during a transaction,
                    # then the _last_firing_only() below will always cause
                    # a sample to be fetched from the one we just switched
                    # to. So anything from the old input cell that might
                    # have happened during this transaction will be suppressed.
                    if self.current_listener is not None:
                        self.current_listener.unlisten()
                    self.current_listener = ba \
                        ._value_stream(trans2) \
                        ._listen_internal(out._node, trans2, out._send, False)

                def __del__(self) -> None:
                    if self.current_listener is not None:
                        self.current_listener.unlisten()

            l1 = bba._value_stream(trans0)._listen(out._node, _Handler())
            return out \
                ._last_firing_only(trans0) \
                ._unsafe_add_cleanup(l1) \
                .hold_lazy(za)

        return Transaction._apply(helper)

    @staticmethod
    def switch_stream(bea: "Cell[Stream[A]]") -> Stream[A]:
        """
        Unwrap a stream inside a cell to give a time-varying stream
        implementation.
        """
        return Transaction._apply(
            lambda trans: Cell._switch_stream(trans, bea))

    @staticmethod
    def _switch_stream(
            trans1: Transaction,
            bea: "Cell[Stream[A]]") -> Stream[A]:
        out: StreamWithSend[A] = StreamWithSend()
        h2 = out._send

        class _Handler:

            def __init__(self) -> None:
                self.current_listener = bea \
                    ._sample_no_trans() \
                    ._listen_internal(out._node, trans1, h2, False)

            def __call__(self, trans2: Transaction, ea: Stream[A]) -> None:
                def run() -> None:
                    if self.current_listener is not None:
                        self.current_listener.unlisten()
                    self.current_listener = ea._listen_internal(
                        out._node, trans2, h2, True)
                trans2.last(run)

            def __del__(self) -> None:
                if self.current_listener is not None:
                    self.current_listener.unlisten()

        h1 = _Handler()
        l1 = bea._updates()._listen_internal(out._node, trans1, h1, False)
        return out._unsafe_add_cleanup(l1)

    def calm(self) -> "Cell[A]":
        """
        Returns a cell that holds the same value as the source cell, but
        ignores updates that don't change value of the cell.
        """
        init = self.sample()
        return self._updates()._calm(init).hold(init)

    def __del__(self) -> None:
        if self._cleanup is not None:
            self._cleanup.unlisten()

    def listen(self, action: Callable[[A],None]) -> Listener:
        """
        Listen for updates to the value of this cell. This is the observer
        pattern. The returned :class:`~sodiumfrp.listener.Listener` has
        an :meth:`~sodiumfrp.listener.Listener.unlisten` method
        to cause the listener to be removed. This is an **operational**
        mechanism for interfacing between the world of I/O and for FRP.

        :param action: the handler to execute when there's a new value.
            You should make no assumptions about what thread you are called
            on, and the handler should not block. You are not allowed
            to use :meth:`CellSink.send` or :meth:`StreamSink.send` in
            the handler. An exception will be thrown, because you are not
            meant to use this to create your own primitives.
        """
        return Transaction._apply(
            lambda trans: self._value_stream(trans).listen(action))


class CellSink(Cell[A]):
    """
    A cell that allows values to be pushed into it, acting as an interface
    between the world of I/O and the world of FRP. Code that exports
    `CellSinks` for read-only use should downcast to
    :class:`~sodiumfrp.primitives.Cell`.
    """

    def __init__(self, init_value: A, f: Callable[[A,A],A] = None) -> None:
        """
        Construct a writable cell with the specified initial value.
        If multiple values are sent in the same transaction, the specified
        function is used to combine them. If the function isn't provided,
        :meth:`send` throws an exception when called multiple times from the
        same transaction.
        """
        super().__init__(StreamSink(f), init_value)

    def send(self, a: A) -> None:
        """
        Send a value, modifying the value of the cell. `send()` may not be
        used inside handlers registered with :meth:`Stream.listen` or
        :meth:`Cell.listen`. An exception will be thrown, because `CellSink`
        is for interfacing I/O to FRP only. You are not meant to use this
        to define your own primitives.

        :param a: value to push into the cell.
        """
        self._stream.send(a)


class LazyCell(Cell[A]):

    def __init__(self, event: Stream[A], lazy_init_value: Lazy[A]) -> None:
        super().__init__(event, None)
        self._lazy_init_value = lazy_init_value

    def _sample_no_trans(self) -> A:
        if (self._value is None) and (self._lazy_init_value is not None):
            self._value = self._lazy_init_value.get()
            self._lazy_init_value = None
        return self._value


class CellLoop(LazyCell[A]):
    """
    A forward reference for a :class:`Cell` equivalent to the `Cell` that is
    referenced.
    """

    def __init__(self) -> None:
        super().__init__(StreamLoop(), None)

    def loop(self, a_out: Cell[A]) -> None:
        """
        Resolve the loop to specify what the `CellLoop` was a forward
        reference to. It must be invoked inside the same transaction as
        the place where the `CellLoop` is used. This requires you to create
        an explicit transaction with
        :meth:`Transaction.run() <sodiumfrp.transaction.Transaction.run>`.
        """
        def handler(trans: Transaction) -> Unit:
            self._stream.loop(a_out._updates())
            self._lazy_init_value = a_out._sample_lazy(trans)
            return UNIT
        Transaction._apply(handler)

    def _sample_no_trans(self) -> A:
        if not self._stream._assigned:
            raise RuntimeError("CellLoop sampled before it was looped")
        return super()._sample_no_trans()

def _print_stack() -> None:
    """ Print full stack trace. format_exc() shows only part of it. """
    stack = traceback.format_stack()[:-2]
    exc = traceback.format_exc()
    stack_lines = "".join(stack).split("\n")[:-1]
    exc_lines = exc.split("\n")
    trace = "\n".join(exc_lines[:1] + stack_lines + exc_lines[1:])
    print(trace)
