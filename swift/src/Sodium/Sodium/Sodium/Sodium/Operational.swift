class Operational
{
    /// <summary>
    ///     A stream that gives the updates/steps for a cell.
    /// </summary>
    /// <typeparam name="T">The type of the values in the cell.</typeparam>
    /// <param name="c"></param>
    /// <returns></returns>
    /// <remarks>
    ///     This is an OPERATIONAL primitive, which is not part of the main Sodium
    ///     API.  It breaks the property of non-detectability of cell steps/updates.
    ///     The rule with this primitive is that you should only use it in functions
    ///     that do not allow the caller to detect the cell updates.
    /// </remarks>
    static func updates<T>(c: Cell<T>) -> Stream<T> { return Transaction.apply(c.updates) }

    /// <summary>
    ///     A stream that is guaranteed to fire once upon listening, giving the current
    ///     value of a cell, and thereafter gives the updates/steps for the cell.
    /// </summary>
    /// <typeparam name="T">The type of the values in the cell.</typeparam>
    /// <param name="c"></param>
    /// <returns></returns>
    /// <remarks>
    ///     This is an OPERATIONAL primitive, which is not part of the main Sodium
    ///     API.  It breaks the property of non-detectability of cell steps/updates.
    ///     The rule with this primitive is that you should only use it in functions
    ///     that do not allow the caller to detect the cell updates.
    /// </remarks>
    static func value<T>(c: Cell<T>) -> Stream<T> { return Transaction.apply(c.value) }

    /// <summary>
    ///     Push each stream event onto a new transaction guaranteed to come before the next externally
    ///     initiated transaction.  Same as <see cref="Split{T, TCollection}(Stream{TCollection})" /> but it works on a single
    ///     value.
    /// </summary>
    /// <typeparam name="T">The type of the stream to defer.</typeparam>
    /// <param name="s">The stream to defer.</param>
    /// <returns>A stream firing the deferred event firings.</returns>
    static func Defer<T>(s: Stream<AnySequence<T>>) -> Stream<T>
    {
        return split(s)
    }

    /// <summary>
    ///     Push each stream event in the list of streams onto a newly created transaction guaranteed
    ///     to come before the next externally initiated transaction.  Note that the semantics
    ///     are such that two different invocations of this method can put stream events into the same
    ///     new transaction, so the resulting stream's events could be simultaneous with
    ///     events output by <see cref="Split{T, TCollection}(Stream{TCollection})" /> or <see cref="Defer{T}(Stream{T})" />
    ///     invoked elsewhere in the code.
    /// </summary>
    /// <typeparam name="T">The collection item type of the stream to split.</typeparam>
    /// <typeparam name="TCollection">The collection type of the stream to split.</typeparam>
    /// <param name="s">The stream to split.</param>
    /// <returns>A stream firing the split event firings.</returns>
    static func split<T>(s: Stream<AnySequence<T>>) -> Stream<T>
    {
        let out = Stream<T>(keepListenersAlive: s.keepListenersAlive)
        let l1 = s.listen(out.node, action: { (trans, aa) in
            var childIx = 0
            for a in aa {
                trans.post(childIx, action: { trans1 in out.send(trans1!, a: a) })
                childIx += 1
            }
        })
        return out.unsafeAddCleanup(l1)
    }
}
