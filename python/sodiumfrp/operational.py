""" Operational primitives that must be used with care. """

from typing import Iterable, TypeVar

from sodiumfrp.stream import Cell, Stream, StreamWithSend
from sodiumfrp.transaction import Transaction

A = TypeVar("A")

# package nz.sodium;
# 
# import java.util.LinkedList;
# 
# 
# /**
#  * Operational primitives that must be used with care.
#  */
# public class Operational {
#     /**
#      * A stream that gives the updates/steps for a {@link Cell}.
#      * <P>
#      * This is an OPERATIONAL primitive, which is not part of the main Sodium
#      * API. It breaks the property of non-detectability of cell steps/updates.
#      * The rule with this primitive is that you should only use it in functions
#      * that do not allow the caller to detect the cell updates.
#      */
#     public static <A> Stream<A> updates(final Cell<A> c)
def updates(c: Cell[A]) -> Stream[A]:
    """
    A stream that gives the updates/steps for a `Cell`.

    This is an OPERATIONAL primitive, which is not part of the main Sodium
    API. It breaks the property of non-detectability of cell steps/updates.
    The rule with this primitive is that you should only use it in functions
    that do not allow the caller to detect the cell updates.
    """
    return c._updates()
#     {
#         return c.updates();
#     }
# 
#     /**
#      * A stream that is guaranteed to fire once in the transaction where value() is invoked, giving
#      * the current value of the cell, and thereafter behaves like {@link #updates(Cell)},
#      * firing for each update/step of the cell's value.
#      * <P>
#      * This is an OPERATIONAL primitive, which is not part of the main Sodium
#      * API. It breaks the property of non-detectability of cell steps/updates.
#      * The rule with this primitive is that you should only use it in functions
#      * that do not allow the caller to detect the cell updates.
#      */
#     public static <A> Stream<A> value(final Cell<A> c)
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
#     {
#         return Transaction.apply(new Lambda1<Transaction, Stream<A>>() {
#         	public Stream<A> apply(Transaction trans) {
#         		return c.value(trans);
#         	}
#         });
#     }
# 
# 	/**
# 	 * Push each event onto a new transaction guaranteed to come before the next externally
# 	 * initiated transaction. Same as {@link #split(Stream)} but it works on a single value.
# 	 */
# 	public static <A> Stream<A> defer(Stream<A> s)
def defer(s: Stream[A]) -> Stream[A]:
    """
    Push each event onto a new transaction guaranteed to come before
    the next externally initiated transaction. Same as `split()` but it
    works on a single value.
    """
    return split(s.map(lambda a: [a]))
# 	{
# 	    return split(s.map(new Lambda1<A,Iterable<A>>() {
# 	        public Iterable<A> apply(A a) {
#                 LinkedList<A> l = new LinkedList<A>();
#                 l.add(a);
#                 return l;
#             }
#         }));
# 	}
# 
# 	/**
# 	 * Push each event in the list onto a newly created transaction guaranteed
# 	 * to come before the next externally initiated transaction. Note that the semantics
# 	 * are such that two different invocations of split() can put events into the same
# 	 * new transaction, so the resulting stream's events could be simultaneous with
# 	 * events output by split() or {@link #defer(Stream)} invoked elsewhere in the code.
# 	 */
#     public static <A, C extends Iterable<A>> Stream<A> split(Stream<C> s) {
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
# 	    final StreamWithSend<A> out = new StreamWithSend<A>();
# 	    Listener l1 = s.listen_(out.node, new TransactionHandler<C>() {
# 	        public void run(Transaction trans, C as) {
    def handler(trans: Transaction, as_: Iterable[A]) -> None:
        child_idx = 0
        for a in as_:
            def run(trans: Transaction, _a: A = a) -> None:
                out._send(trans, _a)
            trans._post(child_idx, run)
            child_idx += 1
# 	            int childIx = 0;
#                 for (final A a : as) {
#                     trans.post_(childIx, new Handler<Transaction>() {
#                         public void run(Transaction trans) {
#                             out.send(trans, a);
#                         }
#                     });
#                     childIx++;
#                 }
# 	        }
# 	    });
    l1 = s._listen(out._node, handler)
    return out._unsafe_add_cleanup(l1)
# 	    return out.unsafeAddCleanup(l1);
#     }
# }
# 
# 
